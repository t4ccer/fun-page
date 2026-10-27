{- | The hydrate-mode interpreter.

Walks the same 'Html' as the renderer, but instead of producing markup it finds the already rendered
elements by their @fp-id@ key, attaches event listeners, and installs effects for the reactive holes.
-}
module FunPage.Hydrate (
  hydrateHtml,
) where

import Control.Exception (throwIO)
import Data.Foldable (for_)
import Data.Text qualified as Text

import FunPage.Html (
  Backend (Backend, backendElement, backendText, backendTextHole),
  ElementInfo (elementBoolAttributes, elementEvents, elementKey, elementTextAttributes),
  Html,
  HtmlError (HtmlError),
  runHtml,
 )
import FunPage.Internal.FFI (
  Element,
  lookupKey,
  onEvent,
  setAttrBool,
  setAttrText,
 )
import FunPage.Internal.HydrationKey (HydrationKey, keyText)
import FunPage.Reactive (Graph, effect, newGraph, runHandler)

-- | Hydrate the current document.
hydrateHtml :: Maybe Int -> Html () -> IO ()
hydrateHtml expectedKeys html = do
  graph <- newGraph
  ((), keys) <- runHtml (mkBackend graph) graph html
  for_ expectedKeys $ \expected ->
    if expected == keys
      then pure ()
      else
        throwIO . HtmlError $
          "fun-page: hydration mismatch: rendered "
            <> Text.pack (show expected)
            <> " keys, hydrated "
            <> Text.pack (show keys)

mkBackend :: Graph -> Backend
mkBackend graph =
  Backend
    { backendText = const (pure ())
    , backendElement = \info children -> do
        let k = elementKey info
        el <- findKey k
        for_ (elementEvents info) $ \(ev, h) -> onEvent el ev (runHandler graph h)
        for_ (elementBoolAttributes info) $ \(k, v) -> effect graph v (setAttrBool el k)
        for_ (elementTextAttributes info) $ \(k, v) -> effect graph v (setAttrText el k)
        children
    , backendTextHole = \k r -> do
        el <- findKey k
        effect graph r (setAttrText el "textContent")
    }

findKey :: HydrationKey -> IO Element
findKey k =
  lookupKey k >>= \case
    Just el -> pure el
    Nothing -> throwIO (HtmlError ("fun-page: hydration mismatch: no element with fp-id=\"" <> keyText k <> "\""))
