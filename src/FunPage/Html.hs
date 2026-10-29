-- | The 'Html' monad and its combinators.
module FunPage.Html (
  -- * Markup
  Html,
  text,
  text_,
  forEachKeyed,

  -- * Document head
  title,
  stylesheet,
  htmlAttribute,

  -- * Elements
  element,
  div_,
  span_,
  p_,
  h1_,
  button_,
  ul_,
  li_,
  section_,
  header_,
  footer_,
  a_,
  label_,
  strong_,
  input_,

  -- * Attributes
  Attribute,
  attribute,
  booleanAttribute,
  property,
  class_,
  classWhen,
  id_,
  for_,
  type_,
  placeholder_,
  href_,
  autofocus,
  hidden,
  checked,
  focusWhen,

  -- * Bindings
  checkedWith,
  valueWith,
  bindChecked,
  bindValue,

  -- * Events
  Event,
  on,
  on_,
  onClick,
  onDoubleClick,
  onKeyDown,
  onBlur,
  eventKey,
  eventTargetValue,
  eventTargetChecked,
) where

import Control.Monad ((>=>))
import Data.IORef (newIORef)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack, callStack)

import FunPage.Internal.FFI (Event)
import FunPage.Internal.FFI qualified as FFI
import FunPage.Internal.Html (
  Backend (backendElement, backendHead, backendKeyed, backendTextHole),
  Child (ChildExclusive, ChildNode),
  ElementInfo (
    ElementInfo,
    elementBoolAttributes,
    elementEvents,
    elementFocus,
    elementKey,
    elementProperties,
    elementTag,
    elementTextAttributes
  ),
  Env (envBackend, envParent),
  HeadItem (HeadHtmlAttribute, HeadStylesheet, HeadTitle),
  Html (Html),
  Parent (Parent),
  addChild,
  allocateKey,
  checkChildren,
  text,
 )
import FunPage.Reactive (
  Handler,
  MonadReactiveRead (readRVar),
  RVar,
  Reactive,
  handlerPerformSyncIO,
  writeRVar,
 )

-- | Reactive text: the content of the enclosing element follows the computation.
--
-- It must be the only content of its element (no wrapper is added for it).
text_ :: (HasCallStack) => Reactive Text -> Html ()
text_ r = Html $ \env -> do
  addChild env (ChildExclusive "text_" callStack)
  backendTextHole (envBackend env) r

-- | A list rendered from a reactive list of values, one item per key.
--
-- When the list changes, items whose key is new are created, items whose key is gone are removed,
-- and the others are moved into the new order. A kept item is never re-rendered, so its DOM and
-- local state survive: an item is rendered once from the value it had when its key appeared, and
-- anything that should change afterwards must be reactive state inside the value (e.g. @RVar@
-- fields). With @id@ as the key function, a changed value is a new key, so the item is recreated
-- instead.
--
-- Rules:
--
-- * Every item is exactly one element.
-- * The list is the only content of its parent element, e.g. @ul_ [] (forEachKeyed ...)@
--   (checked when the site is generated).
-- * Keys are unique; an item with a key already seen in the same list is skipped.
forEachKeyed :: (HasCallStack, Foldable f, Ord k) => Reactive (f a) -> (a -> k) -> (a -> Html ()) -> Html ()
forEachKeyed items key item = Html $ \env -> do
  addChild env (ChildExclusive "forEachKeyed" callStack)
  backendKeyed (envBackend env) items key item

-- | The document title. See 'HeadItem' for where it goes.
title :: Reactive Text -> Html ()
title r = Html $ \env -> backendHead (envBackend env) (HeadTitle r)

-- | Link a stylesheet from the document head.
stylesheet :: Text -> Html ()
stylesheet href = Html $ \env -> backendHead (envBackend env) (HeadStylesheet href)

-- | Set an attribute of @\<html\>@, e.g. @htmlAttribute "lang" "en"@.
htmlAttribute :: Text -> Reactive Text -> Html ()
htmlAttribute k v = Html $ \env -> backendHead (envBackend env) (HeadHtmlAttribute k v)

-- | An attribute of an element.
type Attribute :: Type
data Attribute
  = AttributeText Text (Reactive Text)
  | AttributeBool Text (Reactive Bool)
  | AttributeProperty Text (Reactive Text)
  | AttributeEvent Text (Event -> Handler ())
  | AttributeFocus (Reactive Bool)
  | AttributeMany [Attribute]

flattenAttributes :: [Attribute] -> [Attribute]
flattenAttributes = concatMap $ \case
  AttributeMany as -> flattenAttributes as
  a -> [a]

-- | An attribute with a value.
attribute :: Text -> Reactive Text -> Attribute
attribute = AttributeText

-- | An attribute without a value, such as @disabled@.
booleanAttribute :: Text -> Reactive Bool -> Attribute
booleanAttribute = AttributeBool

hidden :: Reactive Bool -> Attribute
hidden = booleanAttribute "hidden"

-- | A text attribute at build time that is a JS property in the browser, such as @value@.
-- The property is only written when it differs, so e.g. the caret of an input is not reset.
property :: Text -> Reactive Text -> Attribute
property = AttributeProperty

checked :: Reactive Bool -> Attribute
checked = booleanAttribute "checked"

class_ :: Reactive Text -> Attribute
class_ = attribute "class"

-- | A list of classes, each present while its condition holds.
classWhen :: [(Text, Reactive Bool)] -> Attribute
classWhen cs = class_ (Text.unwords . map fst . filter snd <$> traverse (\(c, r) -> (c,) <$> r) cs)

id_ :: Text -> Attribute
id_ = attribute "id" . pure

for_ :: Text -> Attribute
for_ = attribute "for" . pure

type_ :: Text -> Attribute
type_ = attribute "type" . pure

placeholder_ :: Text -> Attribute
placeholder_ = attribute "placeholder" . pure

href_ :: Text -> Attribute
href_ = attribute "href" . pure

autofocus :: Attribute
autofocus = booleanAttribute "autofocus" (pure True)

-- | Focus the element whenever the value becomes 'True'.
focusWhen :: Reactive Bool -> Attribute
focusWhen = AttributeFocus

-- | A checkbox showing a value, and a handler for when the user changes it.
checkedWith :: Reactive Bool -> (Bool -> Handler ()) -> Attribute
checkedWith r h = AttributeMany [checked r, on "change" (eventTargetChecked >=> h)]

-- | An input showing a value, and a handler for every edit by the user.
valueWith :: Reactive Text -> (Text -> Handler ()) -> Attribute
valueWith r h = AttributeMany [property "value" r, on "input" (eventTargetValue >=> h)]

bindChecked :: RVar Bool -> Attribute
bindChecked v = checkedWith (readRVar v) (writeRVar v)

bindValue :: RVar Text -> Attribute
bindValue v = valueWith (readRVar v) (writeRVar v)

-- | Run a handler, given the event object, when the named DOM event fires on the element.
on :: Text -> (Event -> Handler ()) -> Attribute
on = AttributeEvent

-- | 'on' for handlers that do not need the event.
on_ :: Text -> Handler () -> Attribute
on_ ev h = on ev (const h)

onClick :: Handler () -> Attribute
onClick = on_ "click"

onDoubleClick :: Handler () -> Attribute
onDoubleClick = on_ "dblclick"

onBlur :: Handler () -> Attribute
onBlur = on_ "blur"

onKeyDown :: (Event -> Handler ()) -> Attribute
onKeyDown = on "keydown"

-- | @event.key@, or @""@ for events without a key.
eventKey :: Event -> Text
eventKey = FFI.eventKey

-- | @event.target.value@.
eventTargetValue :: Event -> Handler Text
eventTargetValue = handlerPerformSyncIO . FFI.eventTargetValue

-- | @event.target.checked@.
eventTargetChecked :: Event -> Handler Bool
eventTargetChecked = handlerPerformSyncIO . FFI.eventTargetChecked

-- | A generic element.
element :: Text -> [Attribute] -> Html () -> Html ()
element tag attrs0 (Html children) = Html $ \env -> do
  key <- allocateKey env
  let attrs = flattenAttributes attrs0
      info =
        ElementInfo
          { elementTag = tag
          , elementKey = key
          , elementTextAttributes = [(k, v) | AttributeText k v <- attrs]
          , elementBoolAttributes = [(k, v) | AttributeBool k v <- attrs]
          , elementProperties = [(k, v) | AttributeProperty k v <- attrs]
          , elementEvents = [(ev, h) | AttributeEvent ev h <- attrs]
          , elementFocus = [r | AttributeFocus r <- attrs]
          }
  addChild env ChildNode
  parent <- Parent tag <$> newIORef []
  backendElement (envBackend env) info $ do
    children env {envParent = Just parent}
    checkChildren parent

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

ul_ :: [Attribute] -> Html () -> Html ()
ul_ = element "ul"

li_ :: [Attribute] -> Html () -> Html ()
li_ = element "li"

section_ :: [Attribute] -> Html () -> Html ()
section_ = element "section"

header_ :: [Attribute] -> Html () -> Html ()
header_ = element "header"

footer_ :: [Attribute] -> Html () -> Html ()
footer_ = element "footer"

a_ :: [Attribute] -> Html () -> Html ()
a_ = element "a"

label_ :: [Attribute] -> Html () -> Html ()
label_ = element "label"

strong_ :: [Attribute] -> Html () -> Html ()
strong_ = element "strong"

input_ :: [Attribute] -> Html ()
input_ attrs = element "input" attrs (pure ())
