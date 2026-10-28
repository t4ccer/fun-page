{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | The 'Reactive' and 'Handler' monads over the reactive graph.
module FunPage.Internal.Monad where

import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Data.Foldable (traverse_)
import Data.IORef (readIORef)
import Data.Kind (Constraint, Type)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text

import FunPage.Internal.Reactive (Graph, RVar)
import FunPage.Internal.Reactive qualified as R

-- | Operations available both in 'Reactive' and in 'Handler'.
type MonadReactiveRead :: (Type -> Type) -> Constraint
class (Monad m) => MonadReactiveRead m where
  -- | Read a variable. Inside an effect this subscribes the effect to it.
  readRVar :: RVar a -> m a

  -- | Run a computation without subscribing the current effect to anything it reads.
  untracked :: Reactive a -> m a

-- | Monads that can create variables: 'FunPage.Html.Html' and 'Handler'.
type MonadReactiveNew :: (Type -> Type) -> Constraint
class (Monad m) => MonadReactiveNew m where
  newRVar :: a -> m (RVar a)

-- | Read-only reactive computations. Reads are tracked when run inside an effect.
type Reactive :: Type -> Type

type role Reactive representational
newtype Reactive a = Reactive {runReactive :: Graph -> IO a}
  deriving (Functor, Applicative, Monad) via ReaderT Graph IO

instance MonadReactiveRead Reactive where
  readRVar v = Reactive $ \g -> do
    readIORef (R.graphObserver g) >>= traverse_ (R.subscribe g v)
    R.readRVar v
  untracked r = Reactive (`runReactiveUntracked` r)

-- | Run a 'Reactive' computation without subscribing anything.
runReactiveUntracked :: Graph -> Reactive a -> IO a
runReactiveUntracked g r = R.withObserver g Nothing (runReactive r g)

-- | String literals are constant text values.
instance (a ~ Text) => IsString (Reactive a) where
  fromString = pure . Text.pack

-- | Event handlers: can read and write variables. Reads are never tracked.
type Handler :: Type -> Type

type role Handler representational
newtype Handler a = Handler {runHandlerM :: Graph -> IO a}
  deriving (Functor, Applicative, Monad) via ReaderT Graph IO

instance MonadReactiveRead Handler where
  readRVar v = Handler (\_ -> R.readRVar v)
  untracked r = Handler (\g -> runReactiveUntracked g r)

instance MonadReactiveNew Handler where
  newRVar x = Handler (\g -> R.newRVar g x)

-- | Run a handler and every effect made dirty by its writes.
runHandler :: Graph -> Handler a -> IO a
runHandler g h = R.runBatch g (runHandlerM h)

-- | 'R.effect' for a 'Reactive' computation.
reactiveEffect :: Graph -> Reactive a -> (a -> IO ()) -> IO ()
reactiveEffect g r = R.effect g (runReactive r)
