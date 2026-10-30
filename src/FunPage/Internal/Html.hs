{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | The 'Html' monad and the interface its interpreters ('Backend') implement.
module FunPage.Internal.Html where

import Control.Exception (Exception (displayException), throwIO)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text

import FunPage.Internal.FFI (Event)
import FunPage.Internal.HydrationKey (HydrationKey, firstKey, keyToInt)
import FunPage.Internal.HydrationKey qualified as HydrationKey
import FunPage.Internal.Monad (Handler, MonadReactiveNew (newRVar), Reactive)
import FunPage.Internal.Reactive (Graph)
import FunPage.Internal.Reactive qualified as R
import FunPage.Internal.Stack (CallStack, callSiteText)

-- TODO: Performance: detect pure reactive computations and do not hydrate these nodes and do not assign id

-- | An element as seen by a 'Backend'.
type ElementInfo :: Type
data ElementInfo = ElementInfo
  { elementTag :: Text
  , elementKey :: HydrationKey
  , elementTextAttributes :: [(Text, Reactive Text)]
  , elementBoolAttributes :: [(Text, Reactive Bool)]
  -- ^ Attributes without a value at build time; JS properties (@el.hidden@, @el.checked@) in the browser.
  , elementProperties :: [(Text, Reactive Text)]
  -- ^ Attributes at build time; JS properties (@el.value@) in the browser, written only when they differ.
  , elementEvents :: [(Text, Event -> Handler ())]
  , elementFocus :: [Reactive Bool]
  -- ^ Focus the element whenever one of these becomes 'True' (browser only).
  }

-- | Content of the document head, emitted from anywhere in the page.
type HeadItem :: Type
data HeadItem
  = -- | @\<title\>@ at build time, @document.title@ in the browser. The last one wins.
    HeadTitle (Reactive Text)
  | -- | @\<link rel="stylesheet"\>@ at build time; nothing in the browser.
    HeadStylesheet Text
  | -- | An attribute of @\<html\>@. The last one for a name wins.
    HeadHtmlAttribute Text (Reactive Text)

-- | What an interpreter does at each node.
type Backend :: Type
data Backend = Backend
  { backendText :: Text -> IO ()
  , backendElement :: ElementInfo -> IO () -> IO ()
  , backendTextHole :: Reactive Text -> IO ()
  -- ^ Reactive text: the whole content of the enclosing element (checked by the traversal).
  , backendKeyed :: forall f a k. (Foldable f, Ord k) => Reactive (f a) -> (a -> k) -> (a -> Html ()) -> IO ()
  -- ^ A keyed list: the whole content of the enclosing element (checked by the traversal).
  -- The items are 'Html' of their own, which the backend runs with 'runHtml'.
  , backendHead :: HeadItem -> IO ()
  , backendEffect :: forall a. Reactive a -> (a -> IO ()) -> IO ()
  -- ^ Run the sink with the value now and whenever it changes (browser only; a no-op at build time).
  }

type Env :: Type
data Env = Env
  { envBackend :: Backend
  , envGraph :: Graph
  , envNextKey :: IORef HydrationKey
  , envParent :: Maybe Parent
  -- ^ The element whose children are being run, if any.
  }

-- | What the traversal records about the children of an element, to check the content rules.
type Parent :: Type
data Parent = Parent
  { parentTag :: Text
  , parentChildren :: IORef [Child]
  }

type Child :: Type
data Child
  = -- | A text node or an element.
    ChildNode
  | -- | Content that must be alone in its element: the name of the combinator and where it was used.
    ChildExclusive Text CallStack

-- | Where a combinator was called, as @file:line:column@, from the call stack it captured.
callSite :: CallStack -> Text
callSite = Text.pack . callSiteText

-- | Record a child of the current element. Content that must be alone needs an element to be in.
addChild :: Env -> Child -> IO ()
addChild env c = case (envParent env, c) of
  (Just p, _) -> modifyIORef' (parentChildren p) (c :)
  (Nothing, ChildExclusive name cs) ->
    throwIO . HtmlError $ callSite cs <> "fun-page: " <> name <> " must be inside an element, e.g. " <> wrapperFor name <> " [] (" <> name <> " ...)"
  (Nothing, ChildNode) -> pure ()

-- | The element to suggest in errors about content that must be alone.
wrapperFor :: Text -> Text
wrapperFor "forEachKeyed" = "ul_"
wrapperFor _ = "span_"

-- | Check the content rules for the children of an element.
checkChildren :: Parent -> IO ()
checkChildren p = do
  children <- readIORef (parentChildren p)
  case [(name, cs) | ChildExclusive name cs <- reverse children] of
    ((name, cs) : _)
      | length children > 1 ->
          throwIO . HtmlError $
            callSite cs
              <> "fun-page: "
              <> name
              <> " must be the only content of its element, but <"
              <> parentTag p
              <> "> has "
              <> Text.pack (show (length children))
              <> " children; wrap it in an element of its own, e.g. "
              <> wrapperFor name
              <> " [] ("
              <> name
              <> " ...)"
    _ -> pure ()

-- | Markup that can allocate reactive variables.
type Html :: Type -> Type

type role Html representational
newtype Html a = Html (Env -> IO a)
  deriving (Functor, Applicative, Monad) via ReaderT Env IO

-- | String literals are text nodes.
instance (a ~ ()) => IsString (Html a) where
  fromString = text . Text.pack

-- | Misuse of the markup API, detected while the site is generated.
type HtmlError :: Type
newtype HtmlError = HtmlError Text
  deriving stock (Show)

instance Exception HtmlError where
  displayException (HtmlError msg) = Text.unpack msg

-- | Interpret markup with a backend. Returns the number of hydration keys allocated.
runHtml :: Backend -> Graph -> Html a -> IO (a, Int)
runHtml backend graph (Html m) = do
  env <- (\k -> Env backend graph k Nothing) <$> newIORef firstKey
  x <- m env
  keys <- keyToInt <$> readIORef (envNextKey env)
  pure (x, keys)

allocateKey :: Env -> IO HydrationKey
allocateKey env = atomicModifyIORef' (envNextKey env) (\k -> (HydrationKey.nextKey k, k))

instance MonadReactiveNew Html where
  newRVar x = Html $ \env -> R.newRVar (envGraph env) x

-- | A static text node.
text :: Text -> Html ()
text s = Html $ \env ->
  if Text.null s then
    pure ()
  else do
    addChild env ChildNode
    backendText (envBackend env) s
