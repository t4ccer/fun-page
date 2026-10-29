module Main (main) where

import Data.Text qualified as Text

import FunPage.Html (Html, button_, div_, hidden, onClick, span_, text_)
import FunPage.Reactive (modifyRVar, newRVar, readRVar, writeRVar)
import FunPage.Site (emptySite, page, runSite, sitePages)

-- | The standard counter app, with a reset button shown only when the count is non-zero.
counter :: Html ()
counter = do
  count <- newRVar (0 :: Int)
  div_ [] $ do
    button_ [onClick (modifyRVar count (subtract 1))] "-"
    span_ [] (text_ (Text.pack . show <$> readRVar count))
    button_ [onClick (modifyRVar count (+ 1))] "+"
    button_ [onClick (writeRVar count 0), hidden ((== 0) <$> readRVar count)] "reset"

main :: IO ()
main = runSite $ emptySite {sitePages = [page "index.html" counter]}
