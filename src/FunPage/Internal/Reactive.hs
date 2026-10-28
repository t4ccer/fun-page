{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | The reactive graph.
--
-- This is a generic implementation that will be moved later to a separate package
module FunPage.Internal.Reactive where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Exception (SomeException (SomeException), displayException, finally, try)
import Control.Monad (void, when)
import Data.Foldable (for_, traverse_)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import FunPage.Internal.FFI (logError)

-- | Identifies an 'RVar' within its graph.
type VarId :: Type
newtype VarId = VarId Int
  deriving newtype (Eq, Ord)

-- | Identifies an effect within its graph. Effects run in creation order.
type EffectId :: Type
newtype EffectId = EffectId Int
  deriving newtype (Eq, Ord)

-- | How many user-facing writes (and 'runAsyncIO' starts) a variable has seen.
--
-- A pending 'runAsyncIO' only writes its result if the generation is still the one it started with.
type Generation :: Type
newtype Generation = Generation Int
  deriving newtype (Eq)

nextGeneration :: Generation -> Generation
nextGeneration (Generation n) = Generation (n + 1)

-- | Mutable state of the reactive graph.
type Graph :: Type
data Graph = Graph
  { graphNextVar :: IORef VarId
  , graphNextEffect :: IORef EffectId
  , graphSubs :: IORef (Map VarId (Set EffectId))
  -- ^ The effects that read a variable in their last run.
  , graphDeps :: IORef (Map EffectId (Set VarId))
  -- ^ The variables an effect read in its last run (the reverse edges).
  , graphEffects :: IORef (Map EffectId (IO ()))
  -- ^ The action that re-runs an effect.
  , graphObserver :: IORef (Maybe EffectId)
  -- ^ The effect currently running, if any; reads subscribe it.
  , graphDirty :: IORef (Set EffectId)
  -- ^ Effects to run at the end of the current batch.
  , graphOwner :: IORef (Maybe (IORef [IO ()]))
  -- ^ Collects the cleanups of what is created inside 'withOwner'.
  , graphReportError :: Text -> IO ()
  -- ^ Where asynchronous errors (from 'runAsyncIO') go.
  }

-- | A graph that reports errors on the JS console.
newGraph :: IO Graph
newGraph = newGraphWith logError

-- | A graph with a custom error reporter.
newGraphWith :: (Text -> IO ()) -> IO Graph
newGraphWith report = do
  graphNextVar <- newIORef (VarId 0)
  graphNextEffect <- newIORef (EffectId 0)
  graphSubs <- newIORef Map.empty
  graphDeps <- newIORef Map.empty
  graphEffects <- newIORef Map.empty
  graphObserver <- newIORef Nothing
  graphDirty <- newIORef Set.empty
  graphOwner <- newIORef Nothing
  pure Graph {graphReportError = report, ..}

freshVarId :: Graph -> IO VarId
freshVarId g = atomicModifyIORef' (graphNextVar g) (\v@(VarId n) -> (VarId (n + 1), v))

freshEffectId :: Graph -> IO EffectId
freshEffectId g = atomicModifyIORef' (graphNextEffect g) (\e@(EffectId n) -> (EffectId (n + 1), e))

-- | A reactive variable: always holds a value, and tracks who reads it.
type RVar :: Type -> Type

type role RVar representational
data RVar a = RVar
  { rvarId :: VarId
  , rvarRef :: IORef a
  , rvarGeneration :: IORef Generation
  }

newRVar :: Graph -> a -> IO (RVar a)
newRVar g x = RVar <$> freshVarId g <*> newIORef x <*> newIORef (Generation 0)

readRVar :: RVar a -> IO a
readRVar v = readIORef (rvarRef v)

subscribe :: Graph -> RVar a -> EffectId -> IO ()
subscribe g var eff = do
  modifyIORef' (graphSubs g) (Map.insertWith Set.union (rvarId var) (Set.singleton eff))
  modifyIORef' (graphDeps g) (Map.insertWith Set.union eff (Set.singleton (rvarId var)))

unsubscribeAll :: Graph -> EffectId -> IO ()
unsubscribeAll g eff = do
  vars <- atomicModifyIORef' (graphDeps g) (\m -> (Map.delete eff m, Map.findWithDefault Set.empty eff m))
  for_ (Set.toList vars) $ \var ->
    modifyIORef' (graphSubs g) (Map.adjust (Set.delete eff) var)

withObserver :: Graph -> Maybe EffectId -> IO a -> IO a
withObserver g obs act = do
  prev <- readIORef (graphObserver g)
  writeIORef (graphObserver g) obs
  act `finally` writeIORef (graphObserver g) prev

-- | Run the computation and feed the result to the sink
--
-- The passed function is ran immediately with the current value and when the value changes.
--
-- Inside 'withOwner' the effect is disposed together with its owner.
effect :: Graph -> (Graph -> IO a) -> (a -> IO ()) -> IO ()
effect g r sink = do
  eff <- freshEffectId g
  let run = do
        unsubscribeAll g eff
        x <- withObserver g (Just eff) (r g)
        sink x
  modifyIORef' (graphEffects g) (Map.insert eff run)
  onCleanup g $ do
    modifyIORef' (graphEffects g) (Map.delete eff)
    modifyIORef' (graphDirty g) (Set.delete eff)
    unsubscribeAll g eff
  run

-- | Run an action, collecting the cleanups ('onCleanup') of everything it creates, such as effects.
-- Returns the result and an action that runs the cleanups.
withOwner :: Graph -> IO a -> IO (a, IO ())
withOwner g act = do
  cleanups <- newIORef []
  prev <- readIORef (graphOwner g)
  writeIORef (graphOwner g) (Just cleanups)
  x <- act `finally` writeIORef (graphOwner g) prev
  pure (x, readIORef cleanups >>= sequence_)

-- | Register an action to run when the current owner is disposed. Does nothing outside 'withOwner'.
onCleanup :: Graph -> IO () -> IO ()
onCleanup g act = readIORef (graphOwner g) >>= traverse_ (`modifyIORef'` (act :))

-- | Run the dirty effects, lowest 'EffectId' first, until none are left.
flush :: Graph -> IO ()
flush g = do
  -- Effects may make other effects dirty (e.g. a keyed list writes the variables of its items)
  -- and create or dispose effects. Effects created later have higher ids, so a list runs before
  -- the effects of its items, and a disposed effect never runs.
  next <- atomicModifyIORef' (graphDirty g) $ \d ->
    maybe (d, Nothing) (\(e, d') -> (d', Just e)) (Set.minView d)
  for_ next $ \eff -> do
    readIORef (graphEffects g) >>= sequence_ . Map.lookup eff
    flush g

-- | Run a batch of writes (e.g. a handler) untracked, then every effect made dirty by it.
runBatch :: Graph -> (Graph -> IO a) -> IO a
runBatch g act = do
  x <- withObserver g Nothing (act g)
  flush g
  pure x

-- | Number of effects currently subscribed to a variable.
subscriberCount :: Graph -> RVar a -> IO Int
subscriberCount g v = maybe 0 Set.size . Map.lookup (rvarId v) <$> readIORef (graphSubs g)

-- | Write without touching the generation
writeRVarUntracked :: Graph -> RVar a -> a -> IO ()
writeRVarUntracked g v x = do
  writeIORef (rvarRef v) x
  subs <- Map.findWithDefault Set.empty (rvarId v) <$> readIORef (graphSubs g)
  modifyIORef' (graphDirty g) (Set.union subs)

-- | Write a variable
writeRVar :: Graph -> RVar a -> a -> IO ()
writeRVar g v x = do
  modifyIORef' (rvarGeneration v) nextGeneration
  writeRVarUntracked g v x

modifyRVar :: Graph -> RVar a -> (a -> a) -> IO ()
modifyRVar g v f = readRVar v >>= writeRVar g v . f

-- | Run an IO action asynchronously and write its result into the variable.
writeRVarAsyncIO :: Graph -> RVar a -> IO a -> IO ()
writeRVarAsyncIO g v act = do
  myGen <- bumpGeneration v
  void (forkInto g "writeRVarAsyncIO" v myGen act)

-- | Start a new generation of the variable, invalidating every pending asynchronous write to it.
bumpGeneration :: RVar a -> IO Generation
bumpGeneration v = atomicModifyIORef' (rvarGeneration v) $ \gen ->
  let gen' = nextGeneration gen in (gen', gen')

-- | Run the action in a new thread and, if the variable is still at the given generation when it
-- finishes, write its result (as a batch of its own) or report its error. A stale run is dropped
-- silently, including the 'Control.Exception.ThreadKilled' of a cancelled one.
forkInto :: Graph -> Text -> RVar a -> Generation -> IO a -> IO ThreadId
forkInto g name v myGen act = forkIO $ do
  result <- try act
  gen <- readIORef (rvarGeneration v)
  when (gen == myGen) $ case result of
    Left (SomeException e) ->
      graphReportError g ("fun-page: " <> name <> ": " <> Text.pack (displayException e))
    Right x -> runBatch g (\g' -> writeRVarUntracked g' v x)

-- | The state of an asynchronous IO action
type ReactIO :: Type -> Type

type role ReactIO representational
data ReactIO a
  = -- | The latest run finished with this result.
    ReactIOSuccess a
  | -- | The latest run has not finished yet (or, at build time, never started). Holds the stale
    -- result of the run before it, if that one succeeded.
    ReactIOPending (Maybe a)
  | -- | The latest run threw this exception.
    ReactIOException SomeException
  deriving stock (Show, Functor)

-- | Run asynchronous IO action and write the result to the passed 'RVar'
reactAsyncIO ::
  Graph ->
  (forall x. (Graph -> IO x) -> (x -> IO ()) -> IO ()) ->
  (Graph -> IO a) ->
  (a -> IO b) ->
  IO (RVar (ReactIO b))
reactAsyncIO g install input act = do
  out <- newRVar g (ReactIOPending Nothing)
  reactAsyncIOInto g install out input act
  pure out

-- | Run asynchronous IO action and write the result to the passed 'RVar'
reactAsyncIOInto ::
  Graph ->
  (forall x. (Graph -> IO x) -> (x -> IO ()) -> IO ()) ->
  RVar (ReactIO b) ->
  (Graph -> IO a) ->
  (a -> IO b) ->
  IO ()
reactAsyncIOInto g install out input act = do
  running <- newIORef Nothing
  let cancel = do
        void (bumpGeneration out)
        -- 'killThread' may block and we cannot block in DOM callback
        readIORef running >>= traverse_ (void . forkIO . killThread)
        writeIORef running Nothing
  onCleanup g cancel
  install input $ \x -> do
    cancel
    gen <- readIORef (rvarGeneration out)
    readIORef (rvarRef out) >>= \case
      ReactIOSuccess y -> writeRVarUntracked g out (ReactIOPending (Just y))
      ReactIOPending _ -> pure ()
      ReactIOException _ -> writeRVarUntracked g out (ReactIOPending Nothing)
    -- Exceptions become the result instead of being reported. The 'Control.Exception.ThreadKilled'
    -- of a cancelled run is caught here too, but that run is stale, so it is dropped.
    tid <- forkInto g "reactAsyncIO" out gen (either ReactIOException ReactIOSuccess <$> try (act x))
    writeIORef running (Just tid)
