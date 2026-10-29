{-# LANGUAGE JavaScriptFFI #-}

-- | Integration tests of 'Html' against a real DOM, under node with
-- [jsdom](https://github.com/jsdom/jsdom).
--
-- 'testHtml' renders the markup to HTML (as the site generator does), loads it into a fresh jsdom
-- document, hydrates it, and runs the test against that document. A test sees only the DOM, like
-- a user of the page: it finds elements, reads their content, and fires events at them.
--
-- > main = defaultMain $ testCase "counter" . testHtml counter $ do
-- >   inc <- getElementById "inc"
-- >   count <- getElementById "count"
-- >   getTextContent count >>= assertEq "0"
-- >   click inc
-- >   getTextContent count >>= assertEq "1"
--
-- The @jsdom@ package must be resolvable by node from the test executable, e.g. installed with
-- @npm install@ in the project root.
module FunPage.Test (
  -- * Running
  defaultMain,
  HtmlTest,
  testHtml,

  -- * Finding elements
  Element,
  getElementById,
  querySelector,
  querySelectorAll,

  -- * Reading the DOM
  getTextContent,
  getAttribute,
  getValue,
  isChecked,

  -- * Events
  click,
  doubleClick,
  setValue,
  keyDown,

  -- * Assertions
  assertEq,
  assertBool,
) where

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.JS.Prim (JSVal, fromJSArray, fromJSString, isNull, toJSString)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure)
import Test.Tasty.HUnit qualified as HUnit

import FunPage.Internal.FFI (Element (Element))
import FunPage.Internal.Html (Html)
import FunPage.Internal.Hydrate (hydrateHtml)
import FunPage.Internal.Render (renderDocument, renderHtml, renderedKeys)

-- | Run 'TestTree' adjusting for quirks of the JS runtime
defaultMain :: TestTree -> IO ()
defaultMain tree = do
  -- Tasty sizes its output to the terminal, and asks the terminal for its size when stdin is one.
  -- That needs @hReady stdin@, which the JavaScript backend does not implement, so the program
  -- would crash before running any test. 'testMain' makes stdin look like it is not a terminal; the
  -- only difference is that tasty does not align its output to the terminal width.
  js_stdinNotTerminal
  Tasty.defaultMain tree

foreign import javascript unsafe "(() => { if (typeof process !== 'undefined' && process.stdin) process.stdin.isTTY = false; })"
  js_stdinNotTerminal :: IO ()

-- | A test of a page, run by 'testHtml'.
type HtmlTest :: Type -> Type

type role HtmlTest representational
newtype HtmlTest a = HtmlTest (IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Render the markup, load it into a fresh jsdom document, hydrate it, and run the test.
--
-- Every test gets its own document and reactive state. Event handlers run synchronously, so the
-- DOM is up to date as soon as an event function returns; the results of asynchronous IO are not
-- awaited.
testHtml :: Html () -> HtmlTest a -> Assertion
testHtml html (HtmlTest test) = do
  rendered <- renderHtml html
  let document = renderDocument "test.html" "" rendered
  bracket (js_openDocument (toJS document)) js_closeDocument $ \_ -> do
    hydrateHtml (Just (renderedKeys rendered)) html
    void test

-- | The element with the given @id@. Fails the test if there is none.
getElementById :: (HasCallStack) => Text -> HtmlTest Element
getElementById i = withFrozenCallStack $ querySelector ("#" <> i)

-- | The first element matching the CSS selector. Fails the test if there is none.
querySelector :: (HasCallStack) => Text -> HtmlTest Element
querySelector sel = HtmlTest $ do
  el <- js_querySelector (toJS sel)
  if isNull el then
    withFrozenCallStack $ assertFailure ("no element matches " <> show sel)
  else
    pure (Element el)

-- | All elements matching the CSS selector, in document order.
querySelectorAll :: Text -> HtmlTest [Element]
querySelectorAll sel = HtmlTest (map Element <$> (fromJSArray =<< js_querySelectorAll (toJS sel)))

-- | @textContent@ of the element.
getTextContent :: Element -> HtmlTest Text
getTextContent (Element el) = HtmlTest (fromJS <$> js_textContent el)

-- | The value of an attribute, if the element has it.
getAttribute :: Element -> Text -> HtmlTest (Maybe Text)
getAttribute (Element el) k = HtmlTest $ do
  v <- js_getAttribute el (toJS k)
  pure (if isNull v then Nothing else Just (fromJS v))

-- | The @value@ property of an input.
getValue :: Element -> HtmlTest Text
getValue (Element el) = HtmlTest (fromJS <$> js_value el)

-- | The @checked@ property of a checkbox.
isChecked :: Element -> HtmlTest Bool
isChecked (Element el) = HtmlTest (js_checked el)

-- | Click the element, as a user would: checkboxes toggle and fire @change@.
click :: Element -> HtmlTest ()
click (Element el) = HtmlTest (js_click el)

-- | Fire a @dblclick@ event at the element.
doubleClick :: Element -> HtmlTest ()
doubleClick (Element el) = HtmlTest (js_dispatch el (toJS "dblclick"))

-- | Type into an input: set its value and fire an @input@ event.
setValue :: Element -> Text -> HtmlTest ()
setValue (Element el) v = HtmlTest (js_setValue el (toJS v))

-- | Fire a @keydown@ event with the given @key@, e.g. @"Enter"@.
keyDown :: Element -> Text -> HtmlTest ()
keyDown (Element el) k = HtmlTest (js_keyDown el (toJS k))

-- | Fail unless the actual value (first) equals the expected one (second).
assertEq :: (HasCallStack, Eq a, Show a) => a -> a -> HtmlTest ()
assertEq actual expected = HtmlTest (withFrozenCallStack (assertEqual "" expected actual))

-- | Fail with the message unless the condition holds.
assertBool :: (HasCallStack) => String -> Bool -> HtmlTest ()
assertBool msg b = HtmlTest (withFrozenCallStack (HUnit.assertBool msg b))

toJS :: Text -> JSVal
toJS = toJSString . Text.unpack

fromJS :: JSVal -> Text
fromJS = Text.pack . fromJSString

-- The framework's DOM primitives use the global @document@, so a test document replaces it while
-- the test runs and the previous one is restored afterwards.
foreign import javascript unsafe "((html) => { \
  \  const { JSDOM } = require('jsdom'); \
  \  const dom = new JSDOM(html); \
  \  dom.fpPrevious = globalThis.document; \
  \  globalThis.document = dom.window.document; \
  \  return dom; })"
  js_openDocument :: JSVal -> IO JSVal

foreign import javascript unsafe "((dom) => { globalThis.document = dom.fpPrevious; dom.window.close(); })"
  js_closeDocument :: JSVal -> IO ()

foreign import javascript unsafe "((s) => document.querySelector(s))"
  js_querySelector :: JSVal -> IO JSVal

foreign import javascript unsafe "((s) => Array.from(document.querySelectorAll(s)))"
  js_querySelectorAll :: JSVal -> IO JSVal

foreign import javascript unsafe "((el) => String(el.textContent))"
  js_textContent :: JSVal -> IO JSVal

foreign import javascript unsafe "((el, k) => el.getAttribute(k))"
  js_getAttribute :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe "((el) => String(el.value))"
  js_value :: JSVal -> IO JSVal

foreign import javascript unsafe "((el) => !!el.checked)"
  js_checked :: JSVal -> IO Bool

foreign import javascript unsafe "((el) => el.click())"
  js_click :: JSVal -> IO ()

foreign import javascript unsafe "((el, ev) => el.dispatchEvent(new el.ownerDocument.defaultView.MouseEvent(ev, { bubbles: true })))"
  js_dispatch :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "((el, v) => { el.value = v; el.dispatchEvent(new el.ownerDocument.defaultView.Event('input', { bubbles: true })); })"
  js_setValue :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "((el, k) => el.dispatchEvent(new el.ownerDocument.defaultView.KeyboardEvent('keydown', { key: k, bubbles: true })))"
  js_keyDown :: JSVal -> JSVal -> IO ()
