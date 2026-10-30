{-# LANGUAGE CPP #-}

-- | Generating and hydrating a site of several pages.
module FunPage.Site (
  Page,
  page,
  runSite,
  mountPage,
  Site (sitePages, siteOutput, siteClosureCompiler),
  emptySite,
) where

import Control.Exception (SomeException (SomeException), displayException, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Foldable (find, for_)
import Data.Kind (Type)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)
import Text.Read (readMaybe)

import FunPage.Internal.Create (mountHtml)
import FunPage.Internal.FFI (bodyAttribute, getElementById, initialize, isBrowser, logError, scriptPath)
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

type Site :: Type
data Site = Site
  { sitePages :: [Page]
  , siteOutput :: FilePath
  , siteClosureCompiler :: Bool
  }

emptySite :: Site
emptySite =
  Site
    { sitePages = []
    , siteOutput = ".fun-page-out"
    , siteClosureCompiler = False
    }

-- | Under node: generate the site. In the browser: hydrate the current page.
runSite :: Site -> IO ()
runSite site = do
  browser <- isBrowser
  if browser then
    hydrateSite site
  else
    try (generateSite site) >>= \case
      Left (HtmlError msg) -> hPutStrLn stderr (Text.unpack msg) *> exitFailure
      Right () -> pure ()

-- | Write every page and the running program's own bundle (as @app.js@) into the output directory.
generateSite :: Site -> IO ()
generateSite site = do
  let dir = siteOutput site
  createDirectoryIfMissing True dir
  for_ (sitePages site) $ \p -> do
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

-- A MicroHs program is not its own browser bundle: @app.js@ is built separately (with emcc).
#if !defined(__MHS__)
  program <- scriptPath
  let bundle = dir </> "app.js"
  if siteClosureCompiler site then
    closureCompile program bundle
  else do
    code <- ByteString.readFile program
    ByteString.writeFile bundle (stripHashbang code)
  hPutStrLn stderr ("Wrote " <> bundle)
#endif

-- The parts of @filepath@ used here (POSIX paths), so the library does not depend on it: under
-- MicroHs it pulls in mtl, exceptions and os-string.
(</>) :: FilePath -> FilePath -> FilePath
a </> b
  | null a = b
  | last a == '/' = a <> b
  | otherwise = a <> "/" <> b

takeDirectory :: FilePath -> FilePath
takeDirectory path = case dropWhile (/= '/') (reverse path) of
  [] -> "."
  _ : rest -> if null rest then "/" else reverse rest

-- | Minify the running program into the given file with the Closure Compiler.
closureCompile :: FilePath -> FilePath -> IO ()
closureCompile program output = do
  let jsexe = program <> ".jsexe"
      inputs = [jsexe </> "all.js", jsexe </> "all.externs.js"]
  for_ inputs $ \f -> do
    exists <- doesFileExist f
    if exists then
      pure ()
    else
      throwIO (HtmlError ("fun-page: closure compiler: " <> Text.pack f <> " does not exist"))
  hPutStrLn stderr ("Running the Closure Compiler on " <> (jsexe </> "all.js"))
  callProcess "closure-compiler" $
    [ "--language_in=UNSTABLE"
    , "--compilation_level=ADVANCED_OPTIMIZATIONS"
    , "--warning_level=QUIET"
    , "--isolation_mode=IIFE"
    , "--assume_function_wrapper"
    , "--emit_use_strict"
    ]
      <> concat [["--js", f] | f <- inputs]
      <> ["--js_output_file", output]

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

-- | Build the markup into the element with the given id after the element's existing children.
mountPage :: Text.Text -> Html () -> IO ()
mountPage elementId html = do
  initialize
  result <- try $ do
    getElementById elementId >>= \case
      Nothing -> throwIO (HtmlError ("fun-page: no element with id \"" <> elementId <> "\""))
      Just el -> mountHtml el html
  case result of
    Left (SomeException e) -> logError (Text.pack (displayException e))
    Right () -> pure ()

-- | Hydrate the page named by @fp-page@ on @\<body\>@.
hydrateSite :: Site -> IO ()
hydrateSite site = do
  initialize
  result <- try $ do
    name <- bodyAttribute "fp-page"
    count <- bodyAttribute "fp-count"
    case name of
      Nothing -> throwIO (HtmlError "fun-page: <body> has no fp-page attribute")
      Just n -> case find ((== Text.unpack n) . pagePath) (sitePages site) of
        Nothing -> throwIO (HtmlError ("fun-page: no page named \"" <> n <> "\""))
        Just p -> hydrateHtml (readMaybe . Text.unpack =<< count) (pageHtml p)
  case result of
    Left (SomeException e) -> logError (Text.pack (displayException e))
    Right () -> pure ()
