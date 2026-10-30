{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | The DOM primitives on MicroHs in the browser (compiled with emscripten).
--
-- MicroHs passes only numbers and pointers to JavaScript, so JavaScript values live in a table
-- (@globalThis.funPage@, set up by 'initialize') and Haskell holds their indices; strings are UTF-8
-- C strings. Event listeners call into Haskell through the exported 'dispatch', with a stable
-- pointer to the handler (so it also works for handlers of interpreted code, as in the playground,
-- where 'dispatch' is compiled into the interpreter).
--
-- MicroHs pastes each (synchronous) import into emscripten's @EM_ASM@ C macro, so every comma must
-- be inside parentheses (one at the top level, or inside only @[]@ or @{}@, splits the macro's
-- arguments). Interruptible imports have no such restriction.
module FunPage.Internal.FFI.MHS where

import Control.Exception (SomeException (SomeException), displayException, try)
import Control.Exception.JS (JSException (JSException))
import Data.ByteString (ByteString, packCString, useAsCString)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (intPtrToPtr, nullPtr, ptrToIntPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import System.IO.Unsafe (unsafePerformIO)

import FunPage.Internal.HydrationKey (HydrationKey, keyToInt)

-- | A DOM node: usually an element, but also text nodes created by 'createTextNode'.
--
-- An index into the JavaScript-side table of values.
type Element :: Type
newtype Element = Element Int

-- | A DOM event object, passed to handlers. Only valid while its handler runs.
type Event :: Type
newtype Event = Event Int

foreign import javascript "globalThis.funPage = globalThis.funPage || ({\
  \  vals: [null], freeSlots: [], listeners: new Map(),\
  \  put(v) {\
  \    if (v === null || v === undefined) return 0;\
  \    const i = this.freeSlots.length > 0 ? this.freeSlots.pop() : this.vals.length;\
  \    this.vals[i] = v;\
  \    return i;\
  \  },\
  \  get(i) {\
  \    if (i !== 0 && this.vals[i] === undefined) throw new Error(\"fun-page: use of a freed JavaScript handle \" + i);\
  \    return this.vals[i];\
  \  },\
  \  free(i) {\
  \    if (i === 0 || this.vals[i] === undefined) return;\
  \    this.vals[i] = undefined;\
  \    this.freeSlots.push(i);\
  \  },\
  \  str(s) { return s === null || s === undefined ? 0 : stringToNewUTF8(String(s)); },\
  \});"
  js_initialize :: IO ()

foreign import javascript "return typeof document !== \"undefined\" ? 1 : 0;"
  js_isBrowser :: IO Int

foreign import javascript "const target = funPage.get($0);\
  \const name = UTF8ToString($1);\
  \const id = $2;\
  \const f = (e) => {\
  \  const h = funPage.put(e);\
  \  try { _funPageDispatch(id, h); } finally { funPage.free(h); }\
  \};\
  \funPage.listeners.set(id, [target, name, f]);\
  \target.addEventListener(name, f);"
  js_listen :: Int -> CString -> Int -> IO ()

foreign import javascript "const l = funPage.listeners.get($0);\
  \if (l === undefined) return;\
  \l[0].removeEventListener(l[1], l[2]);\
  \funPage.listeners.delete($0);"
  js_unlisten :: Int -> IO ()

foreign import javascript "return funPage.str(document.body ? document.body.getAttribute(UTF8ToString($0)) : null);"
  js_bodyAttribute :: CString -> IO CString

foreign import javascript "return funPage.put(document.querySelector(\"[fp-id='\" + $0 + \"']\"));"
  js_lookupKey :: Int -> IO Int

foreign import javascript "return funPage.put(document.getElementById(UTF8ToString($0)));"
  js_getElementById :: CString -> IO Int

foreign import javascript "const el = funPage.get($0);\
  \const k = UTF8ToString($1);\
  \const v = UTF8ToString($2);\
  \if (el[k] !== v) el[k] = v;"
  js_setProperty :: Int -> CString -> CString -> IO ()

foreign import javascript "funPage.get($0).focus();"
  js_focus :: Int -> IO ()

foreign import javascript "document.title = UTF8ToString($0);"
  js_setDocumentTitle :: CString -> IO ()

foreign import javascript "document.documentElement.setAttribute(UTF8ToString($0), UTF8ToString($1));"
  js_setHtmlAttribute :: CString -> CString -> IO ()

foreign import javascript "const k = funPage.get($0).key; return funPage.str(k === undefined ? \"\" : k);"
  js_eventKey :: Int -> IO CString

foreign import javascript "return funPage.str(String(funPage.get($0).target.value));"
  js_eventTargetValue :: Int -> IO CString

foreign import javascript "return funPage.str(String(funPage.get($0).detail));"
  js_eventDetail :: Int -> IO CString

foreign import javascript "return funPage.get($0).target.checked ? 1 : 0;"
  js_eventTargetChecked :: Int -> IO Int

foreign import javascript "const el = funPage.get($0);\
  \const k = UTF8ToString($1);\
  \if ($2 === 0) el.removeAttribute(k); else el.setAttribute(k, UTF8ToString($2));"
  js_setAttribute :: Int -> CString -> CString -> IO ()

foreign import javascript "return funPage.put(document.createElement(UTF8ToString($0)));"
  js_createElement :: CString -> IO Int

foreign import javascript "return funPage.put(document.createTextNode(UTF8ToString($0)));"
  js_createTextNode :: CString -> IO Int

foreign import javascript "funPage.get($0).appendChild(funPage.get($1));"
  js_appendChild :: Int -> Int -> IO ()

foreign import javascript "const p = funPage.get($0);\
  \const n = funPage.get($1);\
  \const c = p.childNodes[$2];\
  \if (c !== n) p.insertBefore(n, c || null);"
  js_insertNodeAt :: Int -> Int -> Int -> IO ()

foreign import javascript "const n = funPage.get($0); if (n.parentNode) n.parentNode.removeChild(n);"
  js_removeNode :: Int -> IO ()

foreign import javascript "funPage.get($0)[UTF8ToString($1)] = UTF8ToString($2);"
  js_setAttrString :: Int -> CString -> CString -> IO ()

foreign import javascript "funPage.get($0)[UTF8ToString($1)] = $2 !== 0;"
  js_setAttrBool :: Int -> CString -> Int -> IO ()

foreign import javascript "console.error(UTF8ToString($0));"
  js_consoleError :: CString -> IO ()

foreign import javascript "const l = document.createElement(\"link\");\
  \l.rel = \"stylesheet\";\
  \l.href = UTF8ToString($0);\
  \document.head.appendChild(l);"
  js_addStylesheet :: CString -> IO ()

foreign import javascript "return funPage.str(String((0, eval)(UTF8ToString($0))));"
  js_evalJS :: CString -> IO CString

-- The source is copied before the call returns: the C string is freed then.
foreign import javascript interruptible "((s, $c) => Promise.resolve((0, eval)(UTF8ToString(s))).then((v) => $c(funPage.str(String(v)))))"
  js_awaitJS :: CString -> IO CString

-- | Evaluate JavaScript (an expression, in global scope): its value as a string, or what it threw.
evalJS :: Text -> IO (Either Text Text)
evalJS s = tryJS (withText s js_evalJS >>= takeText')

-- | Evaluate JavaScript and wait for the promise it gives: its value as a string, or the
-- rejection. Only the calling thread waits; it can be killed while waiting.
awaitJS :: Text -> IO (Either Text Text)
awaitJS s = tryJS (withText s js_awaitJS >>= takeText')

-- | What the JavaScript threw (MicroHs raises it as a JSException), as @String(e)@.
tryJS :: IO a -> IO (Either Text a)
tryJS io =
  try io >>= \case
    Left (JSException e) -> pure (Left (Text.pack e))
    Right a -> pure (Right a)

withText :: Text -> (CString -> IO a) -> IO a
withText = useAsCString . encodeUtf8

-- | Take a string returned by a primitive (and free it); 'Nothing' for JavaScript null.
takeText :: CString -> IO (Maybe Text)
takeText p
  | p == nullPtr = pure Nothing
  | otherwise = do
      bs <- packCString p :: IO ByteString
      free p
      pure (Just (decodeUtf8 bs))

-- | 'takeText' for strings that are never null.
takeText' :: CString -> IO Text
takeText' p = maybe Text.empty id <$> takeText p

-- | Set up the JavaScript side (the table of values). Must run before any other primitive except
-- 'isBrowser' and 'logError'.
initialize :: IO ()
initialize = js_initialize

foreign export javascript "funPageDispatch" dispatch :: Int -> Int -> IO ()

-- | Called by the JavaScript listener with (a stable pointer to) its handler and the event.
-- Threads it starts or wakes run before this returns, until they wait (e.g. for an asynchronous
-- JavaScript call); they continue from the JavaScript event loop.
dispatch :: Int -> Int -> IO ()
dispatch h e = deRefStablePtr (intToStablePtr h) >>= \act -> act e

stablePtrToInt :: StablePtr a -> Int
stablePtrToInt = fromIntegral . ptrToIntPtr . castStablePtrToPtr

intToStablePtr :: Int -> StablePtr a
intToStablePtr = castPtrToStablePtr . intPtrToPtr . fromIntegral

isBrowser :: IO Bool
isBrowser = (/= 0) <$> js_isBrowser

scriptPath :: IO FilePath
scriptPath = pure ""

bodyAttribute :: Text -> IO (Maybe Text)
bodyAttribute name = withText name js_bodyAttribute >>= takeText

lookupKey :: HydrationKey -> IO (Maybe Element)
lookupKey k = do
  h <- js_lookupKey (keyToInt k)
  pure (if h == 0 then Nothing else Just (Element h))

getElementById :: Text -> IO (Maybe Element)
getElementById i = do
  h <- withText i js_getElementById
  pure (if h == 0 then Nothing else Just (Element h))

-- | Attach an event listener. Returns an action that removes it again.
onEvent :: Element -> Text -> (Event -> IO ()) -> IO (IO ())
onEvent (Element el) ev act = do
  -- Exceptions are caught here rather than in 'dispatch', which may be compiled separately.
  sp <- newStablePtr $ \e ->
    try (act (Event e)) >>= \case
      Left (SomeException err) -> logError (Text.pack (displayException err))
      Right () -> pure ()
  let i = stablePtrToInt sp
  withText ev (\e -> js_listen el e i)
  pure (js_unlisten i >> freeStablePtr sp)

setProperty :: Element -> Text -> Text -> IO ()
setProperty (Element el) k v = withText k (\k' -> withText v (js_setProperty el k'))

focus :: Element -> IO ()
focus (Element el) = js_focus el

setDocumentTitle :: Text -> IO ()
setDocumentTitle s = withText s js_setDocumentTitle

setHtmlAttribute :: Text -> Text -> IO ()
setHtmlAttribute k v = withText k (\k' -> withText v (js_setHtmlAttribute k'))

-- | @event.key@, or @""@ for events without a key.
eventKey :: Event -> Text
eventKey (Event e) = unsafePerformIO (js_eventKey e >>= takeText')

eventTargetValue :: Event -> IO Text
eventTargetValue (Event e) = js_eventTargetValue e >>= takeText'

eventTargetChecked :: Event -> IO Bool
eventTargetChecked (Event e) = (/= 0) <$> js_eventTargetChecked e

eventDetail :: Event -> IO Text
eventDetail (Event e) = js_eventDetail e >>= takeText'

setAttribute :: Element -> Text -> Maybe Text -> IO ()
setAttribute (Element el) k v =
  withText k $ \k' -> case v of
    Nothing -> js_setAttribute el k' nullPtr
    Just v' -> withText v' (js_setAttribute el k')

createElement :: Text -> IO Element
createElement tag = Element <$> withText tag js_createElement

createTextNode :: Text -> IO Element
createTextNode s = Element <$> withText s js_createTextNode

appendChild :: Element -> Element -> IO ()
appendChild (Element p) (Element c) = js_appendChild p c

insertNodeAt :: Element -> Element -> Int -> IO ()
insertNodeAt (Element p) (Element n) = js_insertNodeAt p n

removeNode :: Element -> IO ()
removeNode (Element n) = js_removeNode n

setAttrText :: Element -> Text -> Text -> IO ()
setAttrText (Element el) k v = withText k (\k' -> withText v (js_setAttrString el k'))

setAttrBool :: Element -> Text -> Bool -> IO ()
setAttrBool (Element el) k v = withText k (\k' -> js_setAttrBool el k' (if v then 1 else 0))

logError :: Text -> IO ()
logError s = withText s js_consoleError

-- | Link a stylesheet from the document's head.
addStylesheet :: Text -> IO ()
addStylesheet url = withText url js_addStylesheet
