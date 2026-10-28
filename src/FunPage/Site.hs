-- | Generating and hydrating a site of several pages.
module FunPage.Site (
  Page,
  page,
  runSite,
) where

import Control.Exception (SomeException (SomeException), displayException, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Foldable (find, for_)
import Data.Kind (Type)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import FunPage.Internal.FFI (bodyAttribute, isBrowser, logError, scriptPath)
import FunPage.Internal.Html (Html, HtmlError (HtmlError))
import FunPage.Internal.Hydrate (hydrateHtml)
import FunPage.Internal.Render (renderDocument, renderHtml)

-- | A page of the site: its path below the output directory and its markup.
type Page :: Type
data Page = Page
  { pagePath :: FilePath
  , pageHtml :: Html ()
  }

page :: FilePath -> Html () -> Page
page = Page

-- | Where 'runSite' writes the site under node (relative to the working directory).
outputDirectory :: FilePath
outputDirectory = ".fun-page-out"

-- | Under node: generate the site. In the browser: hydrate the current page.
runSite :: [Page] -> IO ()
runSite pages = do
  browser <- isBrowser
  if browser then
    hydrateSite pages
  else
    try (generateSite outputDirectory pages) >>= \case
      Left (HtmlError msg) -> hPutStrLn stderr (Text.unpack msg) *> exitFailure
      Right () -> pure ()

-- | Write every page and the running program's own bundle (as @app.js@) into the directory.
generateSite :: FilePath -> [Page] -> IO ()
generateSite dir pages = do
  createDirectoryIfMissing True dir
  for_ pages $ \p -> do
    rendered <-
      try (renderHtml (pageHtml p)) >>= \case
        Left (HtmlError msg) -> throwIO (HtmlError (msg <> " (in page " <> Text.pack (pagePath p) <> ")"))
        Right r -> pure r
    let target = dir </> pagePath p
        script = Text.pack (relativeBundle (pagePath p))
    createDirectoryIfMissing True (takeDirectory target)
    ByteString.writeFile
      target
      (Text.Encoding.encodeUtf8 (renderDocument (Text.pack (pagePath p)) script rendered))
    hPutStrLn stderr ("Wrote " <> target)
  bundle <- ByteString.readFile =<< scriptPath
  ByteString.writeFile (dir </> "app.js") (stripHashbang bundle)
  hPutStrLn stderr ("Wrote " <> (dir </> "app.js"))

-- | Path from a page to @app.js@ at the root of the output directory.
relativeBundle :: FilePath -> FilePath
relativeBundle path = concat (replicate depth "../") <> "app.js"
  where
    depth = length (filter (== '/') path)

-- | Drop a leading @#!@ line (the node wrapper that @cabal run@ executes).
stripHashbang :: ByteString -> ByteString
stripHashbang s
  | "#!" `ByteString.isPrefixOf` s = ByteString.drop 1 (ByteString.Char8.dropWhile (/= '\n') s)
  | otherwise = s

-- | Hydrate the page named by @fp-page@ on @\<body\>@.
hydrateSite :: [Page] -> IO ()
hydrateSite pages = do
  result <- try $ do
    name <- bodyAttribute "fp-page"
    count <- bodyAttribute "fp-count"
    case name of
      Nothing -> throwIO (HtmlError "fun-page: <body> has no fp-page attribute")
      Just n -> case find ((== Text.unpack n) . pagePath) pages of
        Nothing -> throwIO (HtmlError ("fun-page: no page named \"" <> n <> "\""))
        Just p -> hydrateHtml (readMaybe . Text.unpack =<< count) (pageHtml p)
  case result of
    Left (SomeException e) -> logError (Text.pack (displayException e))
    Right () -> pure ()
