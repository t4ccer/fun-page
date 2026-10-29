{-# OPTIONS_GHC -Wno-duplicate-exports #-}

-- | Reactive variables, computations and handlers.
module FunPage.Reactive (
  -- * Variables
  RVar,
  newRVar,
  readRVar,
  writeRVar,
  modifyRVar,

  -- * Computations and handlers
  Reactive,
  Handler,
  MonadReactiveRead (..),
  MonadReactiveNew (..),
  handlerPerformSyncIO,

  -- * Asynchronous IO
  writeRVarAsyncIO,
  modifyRVarAsyncIO,
  ReactIO (..),
  reactAsyncIO,
  reactAsyncIOInto,
) where

import FunPage.Internal.Html (Backend (backendEffect), Env (envBackend, envGraph), Html (Html))
import FunPage.Internal.Monad (
  Handler (Handler),
  MonadReactiveNew (newRVar),
  MonadReactiveRead (readRVar, untracked),
  Reactive (Reactive, runReactive),
 )
import FunPage.Internal.Reactive (Graph, RVar, ReactIO (ReactIOException, ReactIOPending, ReactIOSuccess))
import FunPage.Internal.Reactive qualified as R

writeRVar :: RVar a -> a -> Handler ()
writeRVar v a = Handler (\g -> R.writeRVar g v a)

modifyRVar :: RVar a -> (a -> a) -> Handler ()
modifyRVar v f = Handler (\g -> R.modifyRVar g v f)

-- | Run an IO action asynchronously and write its result into the variable.
--
-- A later write to the variable (of any kind) before it finishes cancels it.
writeRVarAsyncIO :: RVar a -> IO a -> Handler ()
writeRVarAsyncIO v act = Handler (\g -> R.writeRVarAsyncIO g v act)

-- | Run an IO action asynchronously and, when it finishes, combine its result with the current value
--
-- A later write to the variable (of any kind) before it finishes cancels it.
modifyRVarAsyncIO :: RVar a -> IO b -> (a -> b -> a) -> Handler ()
modifyRVarAsyncIO v act f = Handler (\g -> R.modifyRVarAsyncIO g v act f)

-- | Run IO synchronously inside a handler.
handlerPerformSyncIO :: IO a -> Handler a
handlerPerformSyncIO = Handler . const

-- | The result of running an IO action on a reactive value, rerun whenever the value changes.
reactAsyncIO :: Reactive a -> (a -> IO b) -> Html (Reactive (ReactIO b))
reactAsyncIO input act = Html $ \env -> do
  readRVar <$> R.reactAsyncIO (envGraph env) (install env) (runReactive input) act

-- | Like 'reactAsyncIO', but writes into a variable
reactAsyncIOInto :: RVar (ReactIO b) -> Reactive a -> (a -> IO b) -> Html ()
reactAsyncIOInto out input act = Html $ \env ->
  R.reactAsyncIOInto (envGraph env) (install env) out (runReactive input) act

-- | The backend's effect (a no-op at build time), for graph-level computations.
install :: Env -> (Graph -> IO x) -> (x -> IO ()) -> IO ()
install env r = backendEffect (envBackend env) (Reactive r)
