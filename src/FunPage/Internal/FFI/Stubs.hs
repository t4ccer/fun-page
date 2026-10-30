{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Stand-ins for the DOM primitives where there is no DOM: native GHC (for documentation) and
-- MicroHs without the @browser@ flag (to generate a static site). Every DOM operation throws.
module FunPage.Internal.FFI.Stubs where

import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO (hPutStrLn, stderr)

import FunPage.Internal.HydrationKey (HydrationKey)

-- | A DOM node: usually an element, but also text nodes created by 'createTextNode'.
type Element :: Type
data Element

-- | A DOM event object, passed to handlers.
type Event :: Type
data Event

unsupported :: String -> IO a
unsupported name = throwIO (ErrorCall ("fun-page: " <> name <> " needs a browser"))

-- | Never: there is no browser.
isBrowser :: IO Bool
isBrowser = pure False

initialize :: IO ()
initialize = pure ()

scriptPath :: IO FilePath
scriptPath = unsupported "scriptPath"

bodyAttribute :: Text -> IO (Maybe Text)
bodyAttribute _ = unsupported "bodyAttribute"

lookupKey :: HydrationKey -> IO (Maybe Element)
lookupKey _ = unsupported "lookupKey"

getElementById :: Text -> IO (Maybe Element)
getElementById _ = unsupported "getElementById"

onEvent :: Element -> Text -> (Event -> IO ()) -> IO (IO ())
onEvent _ _ _ = unsupported "addEventListener"

setProperty :: Element -> Text -> Text -> IO ()
setProperty _ _ _ = unsupported "setProperty"

focus :: Element -> IO ()
focus _ = unsupported "focus"

setDocumentTitle :: Text -> IO ()
setDocumentTitle _ = unsupported "setDocumentTitle"

addStylesheet :: Text -> IO ()
addStylesheet _ = unsupported "addStylesheet"

setHtmlAttribute :: Text -> Text -> IO ()
setHtmlAttribute _ _ = unsupported "setHtmlAttribute"

eventKey :: Event -> Text
eventKey _ = Text.empty

eventTargetValue :: Event -> IO Text
eventTargetValue _ = unsupported "eventTargetValue"

eventTargetChecked :: Event -> IO Bool
eventTargetChecked _ = unsupported "eventTargetChecked"

setAttribute :: Element -> Text -> Maybe Text -> IO ()
setAttribute _ _ _ = unsupported "setAttribute"

createElement :: Text -> IO Element
createElement _ = unsupported "createElement"

createTextNode :: Text -> IO Element
createTextNode _ = unsupported "createTextNode"

appendChild :: Element -> Element -> IO ()
appendChild _ _ = unsupported "appendChild"

insertNodeAt :: Element -> Element -> Int -> IO ()
insertNodeAt _ _ _ = unsupported "insertNodeAt"

removeNode :: Element -> IO ()
removeNode _ = unsupported "removeNode"

setAttrText :: Element -> Text -> Text -> IO ()
setAttrText _ _ _ = unsupported "setAttrText"

setAttrBool :: Element -> Text -> Bool -> IO ()
setAttrBool _ _ _ = unsupported "setAttrBool"

evalJS :: Text -> IO (Either Text Text)
evalJS _ = unsupported "evalJS"

awaitJS :: Text -> IO (Either Text Text)
awaitJS _ = unsupported "awaitJS"

-- | Errors go to stderr.
logError :: Text -> IO ()
logError = hPutStrLn stderr . Text.unpack
