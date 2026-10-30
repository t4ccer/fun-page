-- | The fun-page playground: edit a fun-page program in the browser and run it.
module Main (main) where

import Control.Exception (displayException)
import Control.Monad (unless)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (getArgs)

import FunPage.Html (
  Html,
  attribute,
  bindValue,
  booleanAttribute,
  button_,
  classWhen,
  div_,
  h1_,
  header_,
  id_,
  main_,
  onClick,
  option_,
  pre_,
  select_,
  span_,
  stylesheet,
  text,
  text_,
  title,
 )
import FunPage.Playground.Browser (
  RunResult (runMilliseconds, runOutput, runSucceeded),
  editorValue,
  fetchText,
  initEditor,
  isBrowser,
  runProgram,
  setEditorValue,
 )
import FunPage.Playground.Build (buildCompiler, buildConfigFromEnvironment, copyAssets)
import FunPage.Reactive (
  ReactIO (ReactIOException, ReactIOPending, ReactIOSuccess),
  modifyRVar,
  newRVar,
  reactAsyncIOInto,
  reactIOIntoPending,
  reactIOIsPending,
  readRVar,
  writeRVarAsyncIO,
 )
import FunPage.Site (emptySite, page, runSite, siteOutput, sitePages)

-- | The examples (in @examples/@).
examples :: [Text]
examples = [firstExample, "TodoMVC", "Dice"]

-- | The example shown first.
firstExample :: Text
firstExample = "Counter"

playground :: Html ()
playground = do
  title "fun-page playground"
  stylesheet "playground.css"
  example <- newRVar firstExample
  result <- newRVar (ReactIOPending Nothing)
  reactAsyncIOInto result (readRVar example) $ \name -> do
    initEditor
    content <- fetchText ("examples/" <> name <> ".hs")
    setEditorValue content
    runProgram content

  let run = do
        modifyRVar result reactIOIntoPending
        writeRVarAsyncIO result (fmap ReactIOSuccess (initEditor >> editorValue >>= runProgram))

      busy = reactIOIsPending <$> readRVar result

      output =
        readRVar result <&> \case
          ReactIOSuccess r -> [stripAnsi (runOutput r) | not (Text.null (runOutput r))]
          ReactIOException _ e -> [Text.pack (displayException e)]
          ReactIOPending _ -> []

      failed =
        readRVar result <&> \case
          ReactIOSuccess r -> not (runSucceeded r)
          ReactIOException _ _ -> True
          ReactIOPending _ -> False

  header_ [] $ do
    h1_ [] "fun-page playground"
    select_
      [ id_ "example"
      , bindValue example
      , booleanAttribute "disabled" busy
      ]
      $ mapM_ (\e -> option_ [attribute "value" (pure e)] (text e)) examples
    button_ [id_ "run", onClick run, booleanAttribute "disabled" busy] "Run"
    span_ [id_ "status"] (text_ (describe <$> readRVar result))
  main_ [] $ do
    div_ [id_ "left"] $ do
      -- Filled by ace.
      div_ [id_ "editor"] ""
      pre_
        [id_ "output", classWhen [("error", failed)]]
        (text_ (Text.intercalate "\n" <$> output))
    div_ [id_ "preview"] ""
  where
    describe = \case
      ReactIOPending _ -> "Compiling…"
      ReactIOSuccess r
        | runSucceeded r -> "Running (compiled in " <> seconds (runMilliseconds r) <> " s)"
        | otherwise -> "Failed"
      ReactIOException _ _ -> "Failed"
    seconds ms = Text.pack (show (fromIntegral ms / 1000 :: Double))

-- | Drop the ANSI colour codes from MicroHs's error messages.
stripAnsi :: Text -> Text
stripAnsi t = case Text.breakOn "\ESC[" t of
  (a, "") -> a
  (a, rest) -> a <> stripAnsi (Text.drop 1 (Text.dropWhile (/= 'm') rest))

-- | Generate the site, then (unless given @--site-only@) build the compiler into it.
main :: IO ()
main = do
  let site = emptySite {sitePages = [page "index.html" playground]}
  runSite site
  browser <- isBrowser
  unless browser $ do
    copyAssets examples "." (siteOutput site)
    siteOnly <- elem "--site-only" <$> getArgs
    unless siteOnly $ do
      cfg <- buildConfigFromEnvironment
      buildCompiler cfg (siteOutput site)
