{-# LANGUAGE JavaScriptFFI #-}

-- | The browser side of the playground that is not fun-page: the ace editor, fetching examples and
-- running the compiler (MicroHs in WebAssembly) in the preview frame.
module FunPage.Playground.Browser (
  RunResult (..),
  isBrowser,
  initEditor,
  editorValue,
  setEditorValue,
  fetchText,
  runProgram,
) where

import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.JS.Prim (JSVal, fromJSString, isNull, toJSString)
import Text.Read (readMaybe)

-- | What compiling (and starting) a program gave.
type RunResult :: Type
data RunResult = RunResult
  { runSucceeded :: Bool
  , runMilliseconds :: Int
  -- ^ How long compiling and starting the program took.
  , runOutput :: Text
  -- ^ What the compiler printed (errors).
  }

foreign import javascript unsafe "(() => typeof document !== 'undefined')"
  js_isBrowser :: IO Bool

-- Load ace (once) and make an editor of @#editor@.
foreign import javascript interruptible "(($c) => { \
  \ globalThis.playgroundEditorReady ??= new Promise((ok, err) => { \
  \   const s = document.createElement('script'); \
  \   s.src = 'https://cdnjs.cloudflare.com/ajax/libs/ace/1.36.2/ace.js'; \
  \   s.onload = ok; \
  \   s.onerror = () => err('cannot load the editor (ace)'); \
  \   document.head.appendChild(s); \
  \ }).then(() => { \
  \   const e = ace.edit('editor', { mode: 'ace/mode/haskell', tabSize: 2, useSoftTabs: true, fontSize: 14 }); \
  \   e.commands.addCommand({ name: 'run', bindKey: { win: 'Ctrl-Enter', mac: 'Cmd-Enter' }, \
  \     exec: () => document.getElementById('run').click() }); \
  \   globalThis.playgroundEditor = e; \
  \ }); \
  \ globalThis.playgroundEditorReady.then(() => $c(null), (e) => $c(String(e))); })"
  js_initEditor :: IO JSVal

foreign import javascript unsafe "(() => globalThis.playgroundEditor ? playgroundEditor.getValue() : '')"
  js_editorValue :: IO JSVal

foreign import javascript unsafe "((s) => globalThis.playgroundEditor.setValue(s, -1))"
  js_setEditorValue :: JSVal -> IO ()

foreign import javascript interruptible "((url, $c) => fetch(url) \
  \ .then((r) => r.ok ? r.text() : Promise.reject(url + ': ' + r.status + ' ' + r.statusText)) \
  \ .then((t) => $c('1' + t), (e) => $c('0' + String(e))))"
  js_fetchText :: JSVal -> IO JSVal

-- Run a program in a fresh preview frame
foreign import javascript interruptible "((src, $c) => { \
  \ const wasm = (globalThis.playgroundWasm ??= WebAssembly.compileStreaming(fetch('playground.wasm'))); \
  \ const frame = document.createElement('iframe'); \
  \ frame.title = 'preview'; \
  \ frame.src = 'preview.html'; \
  \ frame.onload = () => { \
  \   const w = frame.contentWindow; \
  \   const s = w.document.createElement('script'); \
  \   s.src = 'playground.js'; \
  \   s.onerror = () => $c('-1\\n0\\ncannot load the compiler (playground.js)'); \
  \   s.onload = async () => { \
  \     const out = []; \
  \     const say = (t) => { if (!t.includes('cannot find config file')) out.push(t); }; \
  \     try { \
  \       const m = await wasm; \
  \       const mod = await w.Playground({ noInitialRun: true, print: say, printErr: say, \
  \         instantiateWasm: (imports, ok) => { \
  \           WebAssembly.instantiate(m, imports).then((inst) => ok(inst, m)); \
  \           return {}; } }); \
  \       mod.FS.writeFile('/Main.hs', src); \
  \       mod.FS.chdir('/'); \
  \       const t = performance.now(); \
  \       const status = mod.callMain(['-q', '-r', 'Main']); \
  \       $c(status + '\\n' + Math.round(performance.now() - t) + '\\n' + out.join('\\n')); \
  \     } catch (e) { \
  \       $c('-1\\n0\\n' + out.concat([String(e)]).join('\\n')); \
  \     } \
  \   }; \
  \   w.document.head.appendChild(s); \
  \ }; \
  \ document.getElementById('preview').replaceChildren(frame); })"
  js_runProgram :: JSVal -> IO JSVal

-- | Whether this runs in the browser (or under node, generating the site).
isBrowser :: IO Bool
isBrowser = js_isBrowser

-- | Make the editor (loading ace the first time).
initEditor :: IO ()
initEditor = do
  r <- js_initEditor
  if isNull r then pure () else throwIO (ErrorCall (fromJSString r))

-- | The editor's text (empty before 'initEditor').
editorValue :: IO Text
editorValue = Text.pack . fromJSString <$> js_editorValue

setEditorValue :: Text -> IO ()
setEditorValue = js_setEditorValue . toJSString . Text.unpack

-- | Fetch a text file (relative to the page).
fetchText :: Text -> IO Text
fetchText url =
  fromJSString <$> js_fetchText (toJSString (Text.unpack url)) >>= \case
    '1' : t -> pure (Text.pack t)
    e -> throwIO (ErrorCall (drop 1 e))

-- | Compile and run a program in a fresh preview frame.
runProgram :: Text -> IO RunResult
runProgram src = do
  r <- Text.pack . fromJSString <$> js_runProgram (toJSString (Text.unpack src))
  case Text.splitOn "\n" r of
    status : ms : out ->
      pure
        RunResult
          { runSucceeded = status == "0"
          , runMilliseconds = fromMaybe 0 (readMaybe (Text.unpack ms))
          , runOutput = Text.intercalate "\n" out
          }
    _ -> throwIO (ErrorCall ("unexpected result from the compiler: " <> Text.unpack r))
