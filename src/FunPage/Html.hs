-- | The 'Html' monad and its combinators.
module FunPage.Html (
  -- * Markup
  Html,
  newRVar,
  text,
  textR,

  -- * Elements
  element,
  div_,
  span_,
  p_,
  h1_,
  button_,

  -- * Attributes
  Attribute,
  attribute,
  booleanAttribute,
  class_,
  on,
  onClick,
  hidden,

  -- * Interpreters
  Backend (..),
  ElementInfo (..),
  HtmlError (..),
  runHtml,
) where

import Control.Exception (Exception (displayException))
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text

import FunPage.Internal.HydrationKey (HydrationKey, firstKey, keyToInt)
import FunPage.Internal.HydrationKey qualified as HydrationKey
import FunPage.Reactive (Graph, Handler, RVar, Reactive, newRVarIO)

-- TODO: Performance: detect pure reactive computations and do not hydrate these nodes and do not assign id

-- | An element as seen by a 'Backend'.
type ElementInfo :: Type
data ElementInfo = ElementInfo
  { elementTag :: Text
  , elementKey :: HydrationKey
  , elementTextAttributes :: [(Text, Reactive Text)]
  , elementBoolAttributes :: [(Text, Reactive Bool)]
  , elementEvents :: [(Text, Handler ())]
  }

-- | What an interpreter does at each node.
type Backend :: Type
data Backend = Backend
  { backendText :: Text -> IO ()
  , backendElement :: ElementInfo -> IO () -> IO ()
  , backendTextHole :: HydrationKey -> Reactive Text -> IO ()
  }

type Env :: Type
data Env = Env
  { envBackend :: Backend
  , envGraph :: Graph
  , envNextKey :: IORef HydrationKey
  }

-- | Markup that can allocate reactive variables.
type Html :: Type -> Type

type role Html representational
newtype Html a = Html (Env -> IO a)
  deriving (Functor, Applicative, Monad) via ReaderT Env IO

-- | String literals are text nodes.
instance a ~ () => IsString (Html a) where
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
  env <- Env backend graph <$> newIORef firstKey
  x <- m env
  keys <- keyToInt <$> readIORef (envNextKey env)
  pure (x, keys)

allocateKey :: Env -> IO HydrationKey
allocateKey env = atomicModifyIORef' (envNextKey env) (\k -> (HydrationKey.nextKey k, k))

-- | Create a reactive variable.
newRVar :: a -> Html (RVar a)
newRVar x = Html $ \env -> newRVarIO (envGraph env) x

-- | A static text node.
text :: Text -> Html ()
text s = Html $ \env -> do
  backendText (envBackend env) s

-- | A reactive text node, rendered as @\<span fp-id="N"\>@ whose content follows the computation.
textR :: Reactive Text -> Html ()
textR r = Html $ \env -> do
  k <- allocateKey env
  backendTextHole (envBackend env) k r

-- | An attribute of an element.
type Attribute :: Type
data Attribute
  = AttributeText Text (Reactive Text)
  | AttributeBool Text (Reactive Bool)
  | AttributeEvent Text (Handler ())

-- | An attribute with a value.
attribute :: Text -> Reactive Text -> Attribute
attribute = AttributeText

-- | An attribute without a value, such as @disabled@.
booleanAttribute :: Text -> Reactive Bool -> Attribute
booleanAttribute = AttributeBool

hidden :: Reactive Bool -> Attribute
hidden = booleanAttribute "hidden"

class_ :: Reactive Text -> Attribute
class_ = attribute "class"

-- | Run a handler when the named DOM event fires on the element.
on :: Text -> Handler () -> Attribute
on = AttributeEvent

onClick :: Handler () -> Attribute
onClick = on "click"

-- | A generic element.
element :: Text -> [Attribute] -> Html () -> Html ()
element tag attrs (Html children) = Html $ \env -> do
  key <- allocateKey env
  let info =
        ElementInfo
          { elementTag = tag
          , elementKey = key
          , elementTextAttributes = [(k, v) | AttributeText k v <- attrs]
          , elementBoolAttributes = [(k, v) | AttributeBool k v <- attrs]
          , elementEvents = [(ev, h) | AttributeEvent ev h <- attrs]
          }
  backendElement (envBackend env) info (children env)

div_ :: [Attribute] -> Html () -> Html ()
div_ = element "div"

span_ :: [Attribute] -> Html () -> Html ()
span_ = element "span"

p_ :: [Attribute] -> Html () -> Html ()
p_ = element "p"

h1_ :: [Attribute] -> Html () -> Html ()
h1_ = element "h1"

button_ :: [Attribute] -> Html () -> Html ()
button_ = element "button"
