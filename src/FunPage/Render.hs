-- | The render-mode interpreter: 'Html' to an HTML string.
module FunPage.Render (
  renderHtml,
  renderDocument,
  escapeText,
  escapeAttribute,
) where

import Control.Monad (unless, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)

import FunPage.Html (
  Backend (Backend, backendElement, backendText, backendTextHole),
  ElementInfo (elementBoolAttributes, elementKey, elementTag, elementTextAttributes),
  Html,
  runHtml,
 )
import FunPage.Internal.HydrationKey (keyText)
import FunPage.Reactive (newGraph, runReactiveUntracked)

-- | Render markup to HTML.
renderHtml :: Html () -> IO (Text, Int)
renderHtml html = do
  graph <- newGraph
  out <- newIORef []
  let emit s = modifyIORef' out (s :)
      backend =
        Backend
          { backendText = emit . escapeText
          , backendElement = \info children -> do
              emit ("<" <> elementTag info)
              emit (" fp-id=\"" <> keyText (elementKey info) <> "\"")
              for (elementTextAttributes info) $ \(k, v) -> do
                value <- runReactiveUntracked graph v
                emit (" " <> k <> "=\"" <> escapeAttribute value <> "\"")
              for (elementBoolAttributes info) $ \(k, v) -> do
                value <- runReactiveUntracked graph v
                when value $ emit (" " <> k)
              emit ">"
              children
              unless (elementTag info `elem` voidElements) $
                emit ("</" <> elementTag info <> ">")
          , backendTextHole = \k r -> do
              s <- runReactiveUntracked graph r
              emit ("<span fp-id=\"" <> keyText k <> "\">" <> escapeText s <> "</span>")
          }
  ((), keys) <- runHtml backend graph html
  body <- Text.concat . reverse <$> readIORef out
  pure (body, keys)

-- | A full document around a rendered body.
renderDocument :: Text -> Text -> Int -> Text -> Text
renderDocument pageName script keys body =
  Text.concat
    [ "<!DOCTYPE html>\n"
    , "<html>\n"
    , "<head>\n"
    , "<meta charset=\"utf-8\">\n"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
    , "</head>\n"
    , "<body fp-page=\"" <> escapeAttribute pageName <> "\" fp-count=\"" <> Text.pack (show keys) <> "\">\n"
    , body
    , "\n<script src=\"" <> escapeAttribute script <> "\" defer></script>\n"
    , "</body>\n"
    , "</html>\n"
    ]

escapeText :: Text -> Text
escapeText = Text.concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  c -> Text.singleton c

escapeAttribute :: Text -> Text
escapeAttribute = Text.concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  c -> Text.singleton c

voidElements :: [Text]
voidElements =
  ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr"]
