{-# LANGUAGE JavaScriptFFI #-}

{- | Every @foreign import javascript@ of the framework lives here, so that the
rest of the code is backend-agnostic and the GHC-internal JS API is used in
exactly one place.
-}
module FunPage.Internal.FFI (
  -- * Environment
  isBrowser,
  scriptPath,
  bodyAttribute,

  -- * DOM
  Element,
  lookupKey,
  onEvent,
  setAttrText,
  setAttrBool,

  -- * Logging
  logError,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.JS.Foreign.Callback (
  Callback,
  OnBlocked (ThrowWouldBlock),
  syncCallback1,
 )
import GHC.JS.Prim (JSVal, fromJSString, isNull, toJSString)

import FunPage.Internal.HydrationKey (HydrationKey, keyToInt)

-- | A DOM element.
type Element :: Type
newtype Element = Element JSVal

foreign import javascript unsafe "(() => typeof document !== 'undefined')"
  js_isBrowser :: IO Bool

foreign import javascript unsafe "(() => process.argv[1])"
  js_scriptPath :: IO JSVal

foreign import javascript unsafe "((n) => document.body ? document.body.getAttribute(n) : null)"
  js_bodyAttribute :: JSVal -> IO JSVal

foreign import javascript unsafe "((k) => document.querySelector('[fp-id=\"' + k + '\"]'))"
  js_lookupKey :: Int -> IO JSVal

foreign import javascript unsafe "((el, ev, cb) => el.addEventListener(ev, cb))"
  js_addEventListener :: JSVal -> JSVal -> Callback (JSVal -> IO ()) -> IO ()

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

{- | Attach a synchronous event listener.
The action must not block; if it does, it throws @thread would block@.
-}
onEvent :: Element -> Text -> IO () -> IO ()
onEvent (Element el) ev act = do
  cb <- syncCallback1 ThrowWouldBlock (const act)
  js_addEventListener el (toJS ev) cb

setAttrText :: Element -> Text -> Text -> IO ()
setAttrText (Element el) k v = js_setAttrString el (toJS k) (toJS v)

setAttrBool :: Element -> Text -> Bool -> IO ()
setAttrBool (Element el) k v = js_setAttrBool el (toJS k) v

-- | Report an error on the JS console.
logError :: Text -> IO ()
logError = js_consoleError . toJS
