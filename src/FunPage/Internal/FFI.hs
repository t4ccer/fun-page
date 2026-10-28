{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
#if defined(javascript_HOST_ARCH)
{-# LANGUAGE JavaScriptFFI #-}
#endif

module FunPage.Internal.FFI where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
#if defined(javascript_HOST_ARCH)
import GHC.JS.Foreign.Callback (
  Callback,
  OnBlocked (ThrowWouldBlock),
  releaseCallback,
  syncCallback1,
 )
import GHC.JS.Prim (JSVal, fromJSString, isNull, jsNull, toJSString)
#else
import Control.Exception (ErrorCall (ErrorCall), throwIO)
import System.IO (hPutStrLn, stderr)
#endif

import FunPage.Internal.HydrationKey (HydrationKey, keyToInt)

-- | A DOM node: usually an element, but also text nodes created by 'createTextNode'.
type Element :: Type
newtype Element = Element JSVal

#if defined(javascript_HOST_ARCH)

foreign import javascript unsafe "(() => typeof document !== 'undefined')"
  js_isBrowser :: IO Bool

foreign import javascript unsafe "(() => process.argv[1])"
  js_scriptPath :: IO JSVal

foreign import javascript unsafe "((n) => document.body ? document.body.getAttribute(n) : null)"
  js_bodyAttribute :: JSVal -> IO JSVal

foreign import javascript unsafe "((k) => document.querySelector('[fp-id=\"' + k + '\"]'))"
  js_lookupKey :: Int -> IO JSVal

foreign import javascript unsafe "((el, k, v) => { if (el[k] !== v) el[k] = v; })"
  js_setProperty :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "((el) => el.focus())"
  js_focus :: JSVal -> IO ()

foreign import javascript unsafe "((s) => { document.title = s; })"
  js_setDocumentTitle :: JSVal -> IO ()

foreign import javascript unsafe "((k, v) => document.documentElement.setAttribute(k, v))"
  js_setHtmlAttribute :: JSVal -> JSVal -> IO ()

-- Pure: @event.key@ is set when the event is created and never changes.
foreign import javascript unsafe "((e) => e.key === undefined ? '' : e.key)"
  js_eventKey :: JSVal -> JSVal

foreign import javascript unsafe "((e) => String(e.target.value))"
  js_eventTargetValue :: JSVal -> IO JSVal

foreign import javascript unsafe "((e) => !!e.target.checked)"
  js_eventTargetChecked :: JSVal -> IO Bool

foreign import javascript unsafe "((el, ev, cb) => el.addEventListener(ev, cb))"
  js_addEventListener :: JSVal -> JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "((el, ev, cb) => el.removeEventListener(ev, cb))"
  js_removeEventListener :: JSVal -> JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "((el, k, v) => { if (v === null) el.removeAttribute(k); else el.setAttribute(k, v); })"
  js_setAttribute :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "((t) => document.createElement(t))"
  js_createElement :: JSVal -> IO JSVal

foreign import javascript unsafe "((s) => document.createTextNode(s))"
  js_createTextNode :: JSVal -> IO JSVal

foreign import javascript unsafe "((p, c) => { p.appendChild(c); })"
  js_appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "((p, n, i) => { const c = p.childNodes[i]; if (c !== n) p.insertBefore(n, c || null); })"
  js_insertNodeAt :: JSVal -> JSVal -> Int -> IO ()

foreign import javascript unsafe "((n) => { if (n.parentNode) n.parentNode.removeChild(n); })"
  js_removeNode :: JSVal -> IO ()

foreign import javascript unsafe "((el, k, s) => { el[k] = s; })"
  js_setAttrString :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "((el, k, b) => { el[k] = b; })"
  js_setAttrBool :: JSVal -> JSVal -> Bool -> IO ()

foreign import javascript unsafe "((s) => console.error(s))"
  js_consoleError :: JSVal -> IO ()

#else

-- Native stubs of the primitives above, so the library also builds with a native GHC.

-- | Stands in for @GHC.JS.Prim.JSVal@: only strings and @null@.
type JSVal :: Type
data JSVal = JSNull | JSString String

toJSString :: String -> JSVal
toJSString = JSString

fromJSString :: JSVal -> String
fromJSString = \case
  JSString str -> str
  JSNull -> ""

isNull :: JSVal -> Bool
isNull = \case
  JSNull -> True
  JSString _ -> False

jsNull :: JSVal
jsNull = JSNull

type Callback :: Type -> Type

type role Callback phantom
data Callback a

type OnBlocked :: Type
data OnBlocked = ThrowWouldBlock

syncCallback1 :: OnBlocked -> (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
syncCallback1 _ _ = unsupported "syncCallback1"

releaseCallback :: Callback a -> IO ()
releaseCallback _ = unsupported "releaseCallback"

-- | Every DOM primitive throws: there is no DOM outside the JavaScript backend.
unsupported :: String -> IO a
unsupported name =
  throwIO (ErrorCall ("fun-page: " <> name <> " needs the JavaScript backend"))

js_isBrowser :: IO Bool
js_isBrowser = pure False

js_scriptPath :: IO JSVal
js_scriptPath = unsupported "scriptPath"

js_bodyAttribute :: JSVal -> IO JSVal
js_bodyAttribute _ = unsupported "bodyAttribute"

js_lookupKey :: Int -> IO JSVal
js_lookupKey _ = unsupported "lookupKey"

js_setProperty :: JSVal -> JSVal -> JSVal -> IO ()
js_setProperty _ _ _ = unsupported "setProperty"

js_focus :: JSVal -> IO ()
js_focus _ = unsupported "focus"

js_setDocumentTitle :: JSVal -> IO ()
js_setDocumentTitle _ = unsupported "setDocumentTitle"

js_setHtmlAttribute :: JSVal -> JSVal -> IO ()
js_setHtmlAttribute _ _ = unsupported "setHtmlAttribute"

js_eventKey :: JSVal -> JSVal
js_eventKey _ = JSString ""

js_eventTargetValue :: JSVal -> IO JSVal
js_eventTargetValue _ = unsupported "eventTargetValue"

js_eventTargetChecked :: JSVal -> IO Bool
js_eventTargetChecked _ = unsupported "eventTargetChecked"

js_addEventListener :: JSVal -> JSVal -> Callback (JSVal -> IO ()) -> IO ()
js_addEventListener _ _ _ = unsupported "addEventListener"

js_removeEventListener :: JSVal -> JSVal -> Callback (JSVal -> IO ()) -> IO ()
js_removeEventListener _ _ _ = unsupported "removeEventListener"

js_setAttribute :: JSVal -> JSVal -> JSVal -> IO ()
js_setAttribute _ _ _ = unsupported "setAttribute"

js_createElement :: JSVal -> IO JSVal
js_createElement _ = unsupported "createElement"

js_createTextNode :: JSVal -> IO JSVal
js_createTextNode _ = unsupported "createTextNode"

js_appendChild :: JSVal -> JSVal -> IO ()
js_appendChild _ _ = unsupported "appendChild"

js_insertNodeAt :: JSVal -> JSVal -> Int -> IO ()
js_insertNodeAt _ _ _ = unsupported "insertNodeAt"

js_removeNode :: JSVal -> IO ()
js_removeNode _ = unsupported "removeNode"

js_setAttrString :: JSVal -> JSVal -> JSVal -> IO ()
js_setAttrString _ _ _ = unsupported "setAttrString"

js_setAttrBool :: JSVal -> JSVal -> Bool -> IO ()
js_setAttrBool _ _ _ = unsupported "setAttrBool"

js_consoleError :: JSVal -> IO ()
js_consoleError = hPutStrLn stderr . fromJSString

#endif

toJS :: Text -> JSVal
toJS = toJSString . Text.unpack

fromJSNullable :: JSVal -> Maybe Text
fromJSNullable v
  | isNull v = Nothing
  | otherwise = Just (Text.pack (fromJSString v))

-- | Whether we run in a browser (@document@ exists) rather than in node.
isBrowser :: IO Bool
isBrowser = js_isBrowser

-- | Path of the running script under node (@process.argv[1]@).
scriptPath :: IO FilePath
scriptPath = fromJSString <$> js_scriptPath

-- | Value of an attribute of @\<body\>@, if present.
bodyAttribute :: Text -> IO (Maybe Text)
bodyAttribute name = fromJSNullable <$> js_bodyAttribute (toJS name)

-- | Find the element carrying @fp-id="k"@.
lookupKey :: HydrationKey -> IO (Maybe Element)
lookupKey k = do
  v <- js_lookupKey (keyToInt k)
  pure (if isNull v then Nothing else Just (Element v))

-- | Attach a synchronous event listener. Returns an action that removes it again.
-- The action must not block; if it does, it throws @thread would block@.
onEvent :: Element -> Text -> (Event -> IO ()) -> IO (IO ())
onEvent (Element el) ev act = do
  cb <- syncCallback1 ThrowWouldBlock (act . Event)
  js_addEventListener el (toJS ev) cb
  pure (js_removeEventListener el (toJS ev) cb *> releaseCallback cb)

-- | Set a JS property to a string, unless it already has that value.
setProperty :: Element -> Text -> Text -> IO ()
setProperty (Element el) k v = js_setProperty el (toJS k) (toJS v)

focus :: Element -> IO ()
focus (Element el) = js_focus el

setDocumentTitle :: Text -> IO ()
setDocumentTitle = js_setDocumentTitle . toJS

-- | Set an attribute of @\<html\>@.
setHtmlAttribute :: Text -> Text -> IO ()
setHtmlAttribute k v = js_setHtmlAttribute (toJS k) (toJS v)

-- | A DOM event object, passed to handlers.
type Event :: Type
newtype Event = Event JSVal

-- | @event.key@, or @""@ for events without a key.
eventKey :: Event -> Text
eventKey (Event e) = Text.pack (fromJSString (js_eventKey e))

eventTargetValue :: Event -> IO Text
eventTargetValue (Event e) = Text.pack . fromJSString <$> js_eventTargetValue e

eventTargetChecked :: Event -> IO Bool
eventTargetChecked (Event e) = js_eventTargetChecked e

-- | Set an attribute with @setAttribute@, or remove it when given 'Nothing'.
setAttribute :: Element -> Text -> Maybe Text -> IO ()
setAttribute (Element el) k v = js_setAttribute el (toJS k) (maybe jsNull toJS v)

createElement :: Text -> IO Element
createElement tag = Element <$> js_createElement (toJS tag)

createTextNode :: Text -> IO Element
createTextNode s = Element <$> js_createTextNode (toJS s)

appendChild :: Element -> Element -> IO ()
appendChild (Element p) (Element c) = js_appendChild p c

-- | Make the node the child at the given index of the parent, moving it there if needed.
insertNodeAt :: Element -> Element -> Int -> IO ()
insertNodeAt (Element p) (Element n) = js_insertNodeAt p n

-- | Remove the node from its parent, if it has one.
removeNode :: Element -> IO ()
removeNode (Element n) = js_removeNode n

setAttrText :: Element -> Text -> Text -> IO ()
setAttrText (Element el) k v = js_setAttrString el (toJS k) (toJS v)

setAttrBool :: Element -> Text -> Bool -> IO ()
setAttrBool (Element el) k v = js_setAttrBool el (toJS k) v

-- | Report an error on the JS console.
logError :: Text -> IO ()
logError = js_consoleError . toJS
