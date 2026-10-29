{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | The render-mode interpreter: 'Html' to an HTML string.
module FunPage.Internal.Render where

import Control.Monad (foldM_, unless, when)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.List (nub)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import FunPage.Internal.Html (
  Backend (Backend, backendEffect, backendElement, backendHead, backendKeyed, backendText, backendTextHole),
  ElementInfo (elementBoolAttributes, elementKey, elementProperties, elementTag, elementTextAttributes),
  HeadItem (HeadHtmlAttribute, HeadStylesheet, HeadTitle),
  Html,
  runHtml,
 )
import FunPage.Internal.HydrationKey (keyText)
import FunPage.Internal.Monad (runReactiveUntracked)
import FunPage.Internal.Reactive (Graph, newGraph)

-- | A rendered page.
type Rendered :: Type
data Rendered = Rendered
  { renderedBody :: Text
  , renderedKeys :: Int
  -- ^ Number of hydration keys allocated.
  , renderedTitle :: Maybe Text
  -- ^ The last 'FunPage.Html.title'.
  , renderedStylesheets :: [Text]
  , renderedHtmlAttributes :: [(Text, Text)]
  -- ^ The last value for every name, in order of first appearance.
  }

-- | Render markup to HTML.
renderHtml :: Html () -> IO Rendered
renderHtml html = do
  graph <- newGraph
  out <- newIORef []
  headItems <- newIORef []
  ((), keys) <- runHtml (mkBackend graph (\s -> modifyIORef' out (s :)) headItems True) graph html
  body <- Text.concat . reverse <$> readIORef out
  items <- reverse <$> readIORef headItems
  titles <- sequence [runReactiveUntracked graph r | HeadTitle r <- items]
  attrs <- sequence [(k,) <$> runReactiveUntracked graph v | HeadHtmlAttribute k v <- items]
  pure
    Rendered
      { renderedBody = body
      , renderedKeys = keys
      , renderedTitle = if null titles then Nothing else Just (last titles)
      , renderedStylesheets = [href | HeadStylesheet href <- items]
      , renderedHtmlAttributes =
          [(k, last [v | (k', v) <- attrs, k' == k]) | k <- nub (map fst attrs)]
      }

mkBackend :: Graph -> (Text -> IO ()) -> IORef [HeadItem] -> Bool -> Backend
mkBackend graph emit headItems withKeys =
  Backend
    { backendText = emit . escapeText
    , backendElement = \info children -> do
        emit ("<" <> elementTag info)
        when withKeys $ emit (" fp-id=\"" <> keyText (elementKey info) <> "\"")
        for_ (elementTextAttributes info) $ \(k, v) -> do
          value <- runReactiveUntracked graph v
          emit (" " <> k <> "=\"" <> escapeAttribute value <> "\"")
        for_ (elementProperties info) $ \(k, v) -> do
          value <- runReactiveUntracked graph v
          emit (" " <> k <> "=\"" <> escapeAttribute value <> "\"")
        for_ (elementBoolAttributes info) $ \(k, v) -> do
          value <- runReactiveUntracked graph v
          when value $ emit (" " <> k)
        emit ">"
        children
        unless (elementTag info `elem` voidElements) $
          emit ("</" <> elementTag info <> ">")
    , backendTextHole = \r -> do
        s <- runReactiveUntracked graph r
        emit (escapeText s)
    , backendKeyed = \items key item -> do
        xs <- runReactiveUntracked graph items
        let go seen x
              | Set.member (key x) seen = pure seen
              | otherwise = do
                  runHtml (mkBackend graph emit headItems False) graph (item x)
                  pure (Set.insert (key x) seen)
        foldM_ go Set.empty xs
    , backendHead = \item -> modifyIORef' headItems (item :)
    , backendEffect = \_ _ -> pure ()
    }

-- | A full document around a rendered page.
renderDocument :: Text -> Text -> Rendered -> Text
renderDocument pageName script Rendered {..} =
  Text.concat
    [ "<!DOCTYPE html>\n"
    , "<html" <> Text.concat [" " <> k <> "=\"" <> escapeAttribute v <> "\"" | (k, v) <- renderedHtmlAttributes] <> ">\n"
    , "<head>\n"
    , "<meta charset=\"utf-8\">\n"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
    , maybe "" (\t -> "<title>" <> escapeText t <> "</title>\n") renderedTitle
    , Text.concat ["<link rel=\"stylesheet\" href=\"" <> escapeAttribute href <> "\">\n" | href <- renderedStylesheets]
    , "</head>\n"
    , "<body fp-page=\"" <> escapeAttribute pageName <> "\" fp-count=\"" <> Text.pack (show renderedKeys) <> "\">\n"
    , renderedBody
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
