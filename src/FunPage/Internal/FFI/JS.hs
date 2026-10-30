{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | The DOM primitives on GHC's JavaScript backend.
module FunPage.Internal.FFI.JS where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.JS.Foreign.Callback (
  Callback,
  OnBlocked (ThrowWouldBlock),
  releaseCallback,
  syncCallback1,
 )
import GHC.JS.Prim (JSVal, fromJSString, isNull, jsNull, toJSString)

import FunPage.Internal.HydrationKey (HydrationKey, keyToInt)

-- | A DOM node: usually an element, but also text nodes created by 'createTextNode'.
type Element :: Type
newtype Element = Element JSVal

-- | A DOM event object, passed to handlers.
type Event :: Type
newtype Event = Event JSVal

foreign import javascript unsafe "(() => typeof document !== 'undefined')"
  js_isBrowser :: IO Bool

foreign import javascript unsafe "(() => process.argv[1])"
  js_scriptPath :: IO JSVal

foreign import javascript unsafe "((n) => document.body ? document.body.getAttribute(n) : null)"
  js_bodyAttribute :: JSVal -> IO JSVal

foreign import javascript unsafe "((k) => document.querySelector('[fp-id=\"' + k + '\"]'))"
  js_lookupKey :: Int -> IO JSVal

foreign import javascript unsafe "((i) => document.getElementById(i))"
  js_getElementById :: JSVal -> IO JSVal

foreign import javascript unsafe "((el, k, v) => { if (el[k] !== v) el[k] = v; })"
  js_setProperty :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "((el) => el.focus())"
  js_focus :: JSVal -> IO ()

foreign import javascript unsafe "((s) => { document.title = s; })"
  js_setDocumentTitle :: JSVal -> IO ()

foreign import javascript unsafe "((k, v) => document.documentElement.setAttribute(k, v))"
  js_setHtmlAttribute :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "((url) => { const l = document.createElement('link'); l.rel = 'stylesheet'; l.href = url; document.head.appendChild(l); })"
  js_addStylesheet :: JSVal -> IO ()

-- Pure: @event.key@ is set when the event is created and never changes.
foreign import javascript unsafe "((e) => e.key === undefined ? '' : e.key)"
  js_eventKey :: JSVal -> JSVal

foreign import javascript unsafe "((e) => String(e.target.value))"
  js_eventTargetValue :: JSVal -> IO JSVal

foreign import javascript unsafe "((e) => String(e.detail))"
  js_eventDetail :: JSVal -> IO JSVal

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

toJS :: Text -> JSVal
toJS = toJSString . Text.unpack

fromJSNullable :: JSVal -> Maybe Text
fromJSNullable v
  | isNull v = Nothing
  | otherwise = Just (Text.pack (fromJSString v))

-- | Whether we run in a browser (@document@ exists) rather than in node.
isBrowser :: IO Bool
isBrowser = js_isBrowser

-- | Nothing to set up.
initialize :: IO ()
initialize = pure ()

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

-- | @document.getElementById@.
getElementById :: Text -> IO (Maybe Element)
getElementById i = do
  v <- js_getElementById (toJS i)
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

-- | Link a stylesheet from the document's head.
addStylesheet :: Text -> IO ()
addStylesheet url = js_addStylesheet (toJS url)

-- | Set an attribute of @\<html\>@.
setHtmlAttribute :: Text -> Text -> IO ()
setHtmlAttribute k v = js_setHtmlAttribute (toJS k) (toJS v)

-- | @event.key@, or @""@ for events without a key.
eventKey :: Event -> Text
eventKey (Event e) = Text.pack (fromJSString (js_eventKey e))

eventTargetValue :: Event -> IO Text
eventTargetValue (Event e) = Text.pack . fromJSString <$> js_eventTargetValue e

eventTargetChecked :: Event -> IO Bool
eventTargetChecked (Event e) = js_eventTargetChecked e

eventDetail :: Event -> IO Text
eventDetail (Event e) = Text.pack . fromJSString <$> js_eventDetail e

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

foreign import javascript unsafe "((s) => { try { return { value: String((0, eval)(s)) }; } catch (e) { return { error: String(e) }; } })"
  js_evalJS :: JSVal -> IO JSVal

foreign import javascript interruptible "((s, $c) => { try { Promise.resolve((0, eval)(s)).then((v) => $c({ value: String(v) }), (e) => $c({ error: String(e) })); } catch (e) { $c({ error: String(e) }); } })"
  js_awaitJS :: JSVal -> IO JSVal

foreign import javascript unsafe "((r) => 'error' in r)"
  js_isError :: JSVal -> Bool

foreign import javascript unsafe "((r) => 'error' in r ? r.error : r.value)"
  js_resultText :: JSVal -> JSVal

-- | Evaluate JavaScript (an expression, in global scope): its value as a string, or what it threw.
evalJS :: Text -> IO (Either Text Text)
evalJS s = jsResult <$> js_evalJS (toJS s)

-- | Evaluate JavaScript and wait for the promise it gives: its value as a string, or the
-- rejection. Only the calling thread waits; it can be killed while waiting.
awaitJS :: Text -> IO (Either Text Text)
awaitJS s = jsResult <$> js_awaitJS (toJS s)

jsResult :: JSVal -> Either Text Text
jsResult r = (if js_isError r then Left else Right) (Text.pack (fromJSString (js_resultText r)))

-- | Report an error on the JS console.
logError :: Text -> IO ()
logError = js_consoleError . toJS
