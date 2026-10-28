{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | The hydrate-mode interpreter.
--
-- Walks the same 'Html' as the renderer, but instead of producing markup it finds the already
-- rendered elements by their @fp-id@ key, attaches event listeners, and installs effects for the
-- reactive holes.
module FunPage.Internal.Hydrate where

import Control.Exception (throwIO)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text qualified as Text

import FunPage.Internal.Create (attachElement, attachHead, keyedList)
import FunPage.Internal.FFI (
  Element,
  lookupKey,
  setAttrText,
 )
import FunPage.Internal.Html (
  Backend (Backend, backendEffect, backendElement, backendHead, backendKeyed, backendText, backendTextHole),
  ElementInfo (elementKey),
  Html,
  HtmlError (HtmlError),
  runHtml,
 )
import FunPage.Internal.HydrationKey (HydrationKey, keyText)
import FunPage.Internal.Monad (reactiveEffect)
import FunPage.Internal.Reactive (Graph, newGraph)

-- | Hydrate the current document.
hydrateHtml :: Maybe Int -> Html () -> IO ()
hydrateHtml expectedKeys html = do
  graph <- newGraph
  parent <- newIORef Nothing
  ((), keys) <- runHtml (mkBackend graph parent) graph html
  for_ expectedKeys $ \expected ->
    if expected == keys then
      pure ()
    else
      throwIO . HtmlError $
        "fun-page: hydration mismatch: rendered "
          <> Text.pack (show expected)
          <> " keys, hydrated "
          <> Text.pack (show keys)

-- | The second argument holds the element whose children are being hydrated.
mkBackend :: Graph -> IORef (Maybe Element) -> Backend
mkBackend graph parent =
  Backend
    { backendText = const (pure ())
    , backendElement = \info children -> do
        let k = elementKey info
        el <- findKey k
        attachElement graph el info
        prev <- readIORef parent
        writeIORef parent (Just el)
        children
        writeIORef parent prev
    , backendTextHole = \r ->
        readIORef parent >>= \case
          Just el -> reactiveEffect graph r (setAttrText el "textContent")
          Nothing -> throwIO (HtmlError "fun-page: text_ must be inside an element")
    , backendKeyed = \items key item ->
        readIORef parent >>= \case
          Just el -> keyedList graph el items key item
          Nothing -> throwIO (HtmlError "fun-page: forEachKeyed must be inside an element")
    , backendHead = attachHead graph
    , backendEffect = reactiveEffect graph
    }

findKey :: HydrationKey -> IO Element
findKey k =
  lookupKey k >>= \case
    Just el -> pure el
    Nothing -> throwIO (HtmlError ("fun-page: hydration mismatch: no element with fp-id=\"" <> keyText k <> "\""))
