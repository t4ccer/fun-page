module Main (main) where

import Data.Text qualified as Text

import FunPage (
  Html,
  MonadReactive (readRVar),
  button_,
  div_,
  hidden,
  modifyRVar,
  newRVar,
  onClick,
  page,
  runSite,
  textR,
  writeRVar,
 )

-- | The standard counter app, with a reset button shown only when the count is non-zero.
counter :: Html ()
counter = do
  count <- newRVar (0 :: Int)
  div_ [] $ do
    button_ [onClick (modifyRVar count (subtract 1))] "-"
    textR (Text.pack . show <$> readRVar count)
    button_ [onClick (modifyRVar count (+ 1))] "+"
    button_ [onClick (writeRVar count 0), hidden ((== 0) <$> readRVar count)] "reset"

main :: IO ()
main = runSite [page "index.html" counter]
