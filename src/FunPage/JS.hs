-- | Calling JavaScript, without writing @foreign import javascript@
-- (which, for instance, the playground cannot compile).
module FunPage.JS (
  jsEval,
  jsAwait,
  JSError (JSError),
) where

import Control.Exception (Exception, throwIO)
import Data.Kind (Type)
import Data.Text (Text)

import FunPage.Internal.FFI qualified as FFI

-- | An error thrown by JavaScript, as a string.
type JSError :: Type
newtype JSError = JSError Text
  deriving stock (Show)

instance Exception JSError

-- | Evaluate JS expression.
jsEval :: Text -> IO Text
jsEval s = FFI.evalJS s >>= either (throwIO . JSError) pure

-- | Evaluate JavaScript and wait for the promise it gives (other threads keep running).
--
-- > jsAwait "fetch('https://example.com/data.txt').then((r) => r.text())"
jsAwait :: Text -> IO Text
jsAwait s = FFI.awaitJS s >>= either (throwIO . JSError) pure
