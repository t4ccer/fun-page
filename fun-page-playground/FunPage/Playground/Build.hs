-- | Building the playground's compiler: MicroHs with fun-page embedded, compiled to WebAssembly.
module FunPage.Playground.Build (
  BuildConfig (..),
  buildConfigFromEnvironment,
  buildCompiler,
  copyAssets,
) where

import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Control.Monad (filterM, unless, when)
import Data.Char (isSpace)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Directory (
  copyFile,
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getModificationTime,
  listDirectory,
  makeAbsolute,
  removeFile,
 )
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (cwd, env), proc, waitForProcess, withCreateProcess)

-- | Where things are.
type BuildConfig :: Type
data BuildConfig = BuildConfig
  { microHs :: FilePath
  -- ^ The MicroHs checkout (with interruptible JavaScript imports: the @js-async@ branch).
  , funPage :: FilePath
  -- ^ The fun-page package (the directory with @fun-page.cabal@).
  , playground :: FilePath
  -- ^ The playground's sources (the directory with @compiler/@.
  , packageDb :: FilePath
  -- ^ MicroHs's package database for the playground (@CABALDIR@), created if missing.
  }

-- | @MICROHS@ (required), and the rest relative to the current directory (the fun-page checkout):
-- @fun-page-playground/@, and @PLAYGROUND_PACKAGES@ (default @dist-newstyle/playground-packages@).
buildConfigFromEnvironment :: IO BuildConfig
buildConfigFromEnvironment = do
  mhs <-
    lookupEnv "MICROHS" >>= \case
      Just d -> makeAbsolute d
      Nothing -> throwIO (ErrorCall "playground: set MICROHS to a MicroHs checkout (the js-async branch)")
  fp <- makeAbsolute "."
  db <- lookupEnv "PLAYGROUND_PACKAGES" >>= makeAbsolute . fromMaybe "dist-newstyle/playground-packages"
  pure BuildConfig {microHs = mhs, funPage = fp, playground = fp </> "fun-page-playground", packageDb = db}

-- | Build the compiler (@playground.js@ and @playground.wasm@) into the output directory.
buildCompiler :: BuildConfig -> FilePath -> IO ()
buildCompiler cfg out = do
  out' <- makeAbsolute out
  let mhs = microHs cfg
      bin = mhs </> "bin"
      runtime = mhs </> "src" </> "runtime"
      db = packageDb cfg
      pkgDir = db </> "mhs-" <> version </> "packages"
      cDir = db </> "playground-c"
      tools = [bin </> t | t <- ["mhs", "mcabal", "cpphs"]]
  createDirectoryIfMissing True db
  createDirectoryIfMissing True cDir
  base <- environment cfg

  -- Build MicroHs tools, from the C in generated/.
  needs (map (\t -> mhs </> "generated" </> t <> ".c") ["mhs", "mcabal", "cpphs"]) tools $
    run base mhs "make" ["bin/mhs", "bin/mcabal", "bin/cpphs"]

  -- Build packages
  needs [bin </> "mhs"] [pkgDir </> "base-" <> version <> ".pkg"] $
    run base (mhs </> "lib") "mcabal" ["-r", "install"]
  for_ ["transformers", "containers"] $ \p -> do
    have <- any (\f -> (p <> "-") `isPrefixOf` f) <$> listDirectoryOr pkgDir
    unless have (run base db "mcabal" ["-r", "install", p])
  funPageSources <- sourcesIn (funPage cfg </> "src")
  needs ((funPage cfg </> "fun-page.cabal") : funPageSources) [pkgDir </> "fun-page-1.0.0.pkg"] $
    run base (funPage cfg) "mcabal" ["-r", "-fbrowser", "install"]

  -- Build web version of MicroHs
  let packages = "base:transformers:containers:ghc-compat:array-mhs:fun-page"
      cFile = cDir </> "playground.c"
  microHsSources <- (<>) <$> sourcesIn (mhs </> "src" </> "MicroHs") <*> sourcesIn (mhs </> "mhs")
  installed <- map (pkgDir </>) <$> listDirectoryOr pkgDir
  needs ((playground cfg </> "compiler" </> "Compiler.hs") : microHsSources <> installed) [cFile] $
    run
      base
      (playground cfg </> "compiler")
      "mhs"
      [ "-z"
      , "-i"
      , "-i" <> mhs </> "mhs"
      , "-i" <> mhs </> "src"
      , "-i."
      , "Compiler"
      , "-o" <> cFile
      , "--embed-packages"
      , packages
      , "--embed-ffis"
      , packages
      ]
  runtimeSources <- sourcesIn runtime
  needs (cFile : runtimeSources) [out' </> "playground.js", out' </> "playground.wasm"] $
    run base cDir "emcc" $
      [ "-O3"
      , "-I" <> runtime
      , "-I" <> runtime </> "unix"
      , "-DUSE_SYSTEM_RAW"
      , -- ghc-compat imports waitpid without its header.
        "-include"
      , "sys/wait.h"
      , cFile
      , runtime </> "eval.c"
      , runtime </> "main.c"
      , "-lm"
      , "-sMODULARIZE=1"
      , "-sEXPORT_NAME=Playground"
      , "-sENVIRONMENT=web"
      , "-sEXPORTED_RUNTIME_METHODS=FS,callMain,stringToNewUTF8,UTF8ToString"
      , "-sFORCE_FILESYSTEM=1"
      , "-sALLOW_MEMORY_GROWTH"
      , "-sTOTAL_STACK=5MB"
      , "-Wno-address-of-packed-member"
      , "-o"
      , out' </> "playground.js"
      ]
  hPutStrLn stderr ("Wrote " <> out </> "playground.js")
  hPutStrLn stderr ("Wrote " <> out </> "playground.wasm")

-- | Copy static assets and the examples for mounting in the preview.
copyAssets :: [Text] -> FilePath -> FilePath -> IO ()
copyAssets exampleNames sources out = do
  let staticDir = sources </> "fun-page-playground" </> "static"
      exampleDir = sources </> "examples"
  statics <- listDirectory staticDir
  for_ statics $ \s -> do
    let f = out </> s
    hPutStrLn stderr ("Wrote " <> f)
    copyFile (staticDir </> s) f
  createDirectoryIfMissing True (out </> "examples")
  let examples = map ((<> ".hs") . Text.unpack) exampleNames
  oldExamples <- filter (".hs" `isSuffixOf`) <$> listDirectory (out </> "examples")
  for_ (filter (`notElem` examples) oldExamples) $ \e -> do
    let f = out </> "examples" </> e
    removeFile f
    hPutStrLn stderr ("Removed " <> f)
  for_ examples $ \e -> do
    source <- Text.IO.readFile (exampleDir </> e)
    adapted <- either (throwIO . ErrorCall . (("playground: " <> e <> ": ") <>)) pure (mountExample source)
    let f = out </> "examples" </> e
    Text.IO.writeFile f adapted
    hPutStrLn stderr ("Wrote " <> f)

-- | Adapt the single-page entry point and its Site import.
mountExample :: Text -> Either String Text
mountExample source = do
  let ls = Text.lines source

      isSiteImport line = case Text.words line of
        "import" : "FunPage.Site" : _ -> True
        _ -> False

      isMain line = case Text.words line of
        "main" : "=" : _ -> True
        _ -> False

      splitMain = Text.split (\c -> isSpace c || c `elem` ("$={}[]" :: String))
  unless (length (filter isSiteImport ls) == 1) (Left "expected one FunPage.Site import")
  component <- case filter isMain ls of
    [line] -> case filter (not . Text.null) (splitMain line) of
      ["main", "runSite", "emptySite", "sitePages", "page", "\"index.html\"", comp] -> Right comp
      ["main", "emptySite", "sitePages", "page", "\"index.html\"", comp] -> Right comp
      _ -> Left "expected main = runSite $ emptySite {sitePages = [page \"index.html\" component]}"
    _ -> Left "expected one main binding"
  let adapt line
        | isSiteImport line = "import FunPage.Site (mountPage)"
        | isMain line = "main = mountPage \"app\" " <> component
        | otherwise = line
  pure ("{-# LANGUAGE GHC2021, OverloadedStrings, LambdaCase #-}\n" <> Text.unlines (map adapt ls))

-- | The MicroHs version (of the checkout's @generated/@ compiler and @lib/@).
version :: String
version = "0.16.12.0"

-- | The environment for MicroHs's tools: its @bin/@ first on the path, and the package database.
environment :: BuildConfig -> IO [(String, String)]
environment cfg = do
  e <- getEnvironment
  let path = microHs cfg </> "bin" <> maybe "" (":" <>) (lookup "PATH" e)
      set =
        [ ("PATH", path)
        , ("MHSDIR", microHs cfg)
        , ("CABALDIR", packageDb cfg)
        , ("MHSPKG", packageDb cfg </> "mhs-" <> version)
        ]
  pure (set <> [kv | kv@(k, _) <- e, k `notElem` map fst set])

-- | Run the action unless all outputs exist and are newer than every input.
needs :: [FilePath] -> [FilePath] -> IO () -> IO ()
needs inputs outputs act = do
  fresh <- upToDate
  unless fresh act
  where
    upToDate = do
      present <- and <$> traverse doesFileExist outputs
      if not present then
        pure False
      else do
        inputTimes <- traverse getModificationTime =<< filterM doesFileExist inputs
        oldestOutput <- minimum <$> traverse getModificationTime outputs
        pure (all (< oldestOutput) inputTimes)

-- | Run a command in a directory
run :: [(String, String)] -> FilePath -> String -> [String] -> IO ()
run e dir cmd args = do
  hPutStrLn stderr ("playground: (in " <> dir <> ") " <> unwords (cmd : args))
  code <- withCreateProcess (proc cmd args) {cwd = Just dir, env = Just e} $ \_ _ _ -> waitForProcess
  when (code /= ExitSuccess) $
    throwIO (ErrorCall ("playground: " <> cmd <> " failed (" <> showCode code <> ")"))
  where
    showCode = \case
      ExitFailure n -> "exit code " <> show n
      ExitSuccess -> "success"

-- | Haskell and C files below a directory.
sourcesIn :: FilePath -> IO [FilePath]
sourcesIn dir = do
  entries <- listDirectoryOr dir
  concat <$> traverse visit entries
  where
    visit name = do
      let p = dir </> name
      isDir <- doesDirectoryExist p
      if isDir then
        sourcesIn p
      else
        pure [p | any (`isSuffixOf` name) [".hs", ".c", ".h"]]

listDirectoryOr :: FilePath -> IO [FilePath]
listDirectoryOr dir = do
  exists <- doesDirectoryExist dir
  if exists then listDirectory dir else pure []

(</>) :: FilePath -> FilePath -> FilePath
a </> b = a <> "/" <> b

infixr 5 </>
