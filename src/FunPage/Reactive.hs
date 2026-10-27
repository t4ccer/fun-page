-- | The reactive graph.
module FunPage.Reactive (
  -- * Graph
  Graph,
  newGraph,
  newGraphWith,
  effect,
  runHandler,
  runReactiveUntracked,
  subscriberCount,

  -- * Variables
  RVar,
  newRVarIO,

  -- * Identifiers
  VarId,
  EffectId,
  Generation,

  -- * Monads
  Reactive (..),
  Handler (..),
  MonadReactive (..),
  writeRVar,
  modifyRVar,
  bindIO,
  unsafeHandlerPerformIO,
) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException (SomeException), displayException, finally, try)
import Control.Monad (void)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Data.Foldable (for_, traverse_)
import Data.IORef (
  IORef,
  atomicModifyIORef',
  modifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
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

{- | How many user-facing writes (and 'bindIO' starts) a variable has seen.

A pending 'bindIO' only writes its result if the generation is still the one it started with.
-}
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
  , graphReportError :: Text -> IO ()
  -- ^ Where asynchronous errors (from 'bindIO') go.
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

newRVarIO :: Graph -> a -> IO (RVar a)
newRVarIO g x = RVar <$> freshVarId g <*> newIORef x <*> newIORef (Generation 0)

-- | Operations available both in 'Reactive' and in 'Handler'.
type MonadReactive :: (Type -> Type) -> Constraint
class Monad m => MonadReactive m where
  -- | Read a variable. Inside an effect this subscribes the effect to it.
  readRVar :: RVar a -> m a

  -- | Run a computation without subscribing the current effect to anything it reads.
  untracked :: Reactive a -> m a

-- | Read-only reactive computations. Reads are tracked when run inside an effect.
type Reactive :: Type -> Type

type role Reactive representational
newtype Reactive a = Reactive {runReactive :: Graph -> IO a}
  deriving (Functor, Applicative, Monad) via ReaderT Graph IO

instance MonadReactive Reactive where
  readRVar v = Reactive $ \g -> do
    readIORef (graphObserver g) >>= traverse_ (subscribe g (rvarId v))
    readIORef (rvarRef v)
  untracked r = Reactive (`runReactiveUntracked` r)

-- | String literals are constant text values.
instance a ~ Text => IsString (Reactive a) where
  fromString = pure . Text.pack

-- | Event handlers: can read and write variables. Reads are never tracked.
type Handler :: Type -> Type

type role Handler representational
newtype Handler a = Handler {runHandlerM :: Graph -> IO a}
  deriving (Functor, Applicative, Monad) via ReaderT Graph IO

instance MonadReactive Handler where
  readRVar v = Handler (const (readIORef (rvarRef v)))
  untracked r = Handler (`runReactiveUntracked` r)

subscribe :: Graph -> VarId -> EffectId -> IO ()
subscribe g var eff = do
  modifyIORef' (graphSubs g) (Map.insertWith Set.union var (Set.singleton eff))
  modifyIORef' (graphDeps g) (Map.insertWith Set.union eff (Set.singleton var))

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

-- | Run a 'Reactive' computation without subscribing anything.
runReactiveUntracked :: Graph -> Reactive a -> IO a
runReactiveUntracked g r = withObserver g Nothing (runReactive r g)

-- | Run the computation and feed the result to the sink, now and again whenever a variable it read is written.
effect :: Graph -> Reactive a -> (a -> IO ()) -> IO ()
effect g r sink = do
  eff <- freshEffectId g
  let run = do
        unsubscribeAll g eff
        x <- withObserver g (Just eff) (runReactive r g)
        sink x
  modifyIORef' (graphEffects g) (Map.insert eff run)
  run

-- | Run a handler and every effect made dirty by its writes.
runHandler :: Graph -> Handler () -> IO ()
runHandler g h = do
  withObserver g Nothing (runHandlerM h g)
  flush g

-- | Run the dirty effects.
flush :: Graph -> IO ()
flush g = do
  dirty <- atomicModifyIORef' (graphDirty g) (Set.empty,)
  effects <- readIORef (graphEffects g)
  for_ (Set.toAscList dirty) $ \eff -> sequence_ (Map.lookup eff effects)

-- | Number of effects currently subscribed to a variable.
subscriberCount :: Graph -> RVar a -> IO Int
subscriberCount g v = maybe 0 Set.size . Map.lookup (rvarId v) <$> readIORef (graphSubs g)

-- | Write without touching the generation
writeRaw :: Graph -> RVar a -> a -> IO ()
writeRaw g v x = do
  writeIORef (rvarRef v) x
  subs <- Map.findWithDefault Set.empty (rvarId v) <$> readIORef (graphSubs g)
  modifyIORef' (graphDirty g) (Set.union subs)

-- | Write a variable; also cancels any pending 'bindIO' on it.
writeRVar :: RVar a -> a -> Handler ()
writeRVar v x = Handler $ \g -> do
  modifyIORef' (rvarGeneration v) nextGeneration
  writeRaw g v x

modifyRVar :: RVar a -> (a -> a) -> Handler ()
modifyRVar v f = readRVar v >>= writeRVar v . f

-- TODO: Do we want to cancel other computation that writes to the same RVar if it is still running?

-- TODO: Maybe something more general could work
-- bindIO :: IO a -> (a -> Handler ()) -> Handler ()
-- that way the IO result can be split into multiple RVars

-- | Run an IO action asynchronously and write its result into the variable.
bindIO :: RVar a -> IO a -> Handler ()
bindIO v act = Handler $ \g -> do
  myGen <- atomicModifyIORef' (rvarGeneration v) (\gen -> let gen' = nextGeneration gen in (gen', gen'))
  void $ forkIO $ do
    result <- try act
    case result of
      Left (SomeException e) -> graphReportError g ("fun-page: bindIO: " <> Text.pack (displayException e))
      Right x -> do
        gen <- readIORef (rvarGeneration v)
        if gen == myGen
          then runHandler g (Handler (\g' -> writeRaw g' v x))
          else pure ()

-- | Run IO synchronously inside a handler.
unsafeHandlerPerformIO :: IO a -> Handler a
unsafeHandlerPerformIO = Handler . const
