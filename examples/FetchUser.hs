{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- | Loading data from a public API (<https://jsonplaceholder.typicode.com>)
module Main (main) where

import Control.Exception (SomeException (SomeException), displayException, throwIO)
import Control.Monad (unless)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.JS.Prim (JSVal, fromJSString, isNull, toJSString)

import FunPage.Html
import FunPage.Reactive
import FunPage.Site (emptySite, page, runSite, sitePages)

main :: IO ()
main = runSite $ emptySite {sitePages = [page "index.html" page']}

page' :: Html ()
page' = do
  h1_ [] "Users from JSONPlaceholder"

  -- With 'runAsyncIO': the handler starts the request, and its result is written into an 'RVar'
  -- when it arrives. Any later write to that 'RVar' (here: the next click on Load) makes a pending
  -- result stale, so it is dropped; the request itself still runs to completion.
  section_ [] $ do
    uid <- newRVar (1 :: Int)
    name <- newRVar ("Not loaded" :: Text)
    let load = do
          i <- readRVar uid
          writeRVar name "Loading…"
          writeRVarAsyncIO name (fetchUserName i)
    p_ [] "writeRVarAsyncIO: click Load to fetch the selected user."
    div_ [] $ do
      button_ [onClick (modifyRVar uid (\i -> max 1 (i - 1)))] "-"
      span_ [] (text_ (("user " <>) . showT <$> readRVar uid))
      button_ [onClick (modifyRVar uid (\i -> min 11 (i + 1)))] "+"
      button_ [onClick load] "Load"
    p_ [] (text_ (readRVar name))

  -- With 'reactAsyncIO': the request follows the user id by itself. Changing the id cancels the
  -- running request, and the result is pending until the latest one finishes. JSONPlaceholder has
  -- users 1 to 10; the id goes up to 11 to show a failed request.
  section_ [] $ do
    uid <- newRVar (1 :: Int)
    name <- reactAsyncIO (readRVar uid) fetchUserName
    p_ [] "reactAsyncIO: the selected user is fetched automatically."
    div_ [] $ do
      button_ [onClick (modifyRVar uid (\i -> max 1 (i - 1)))] "-"
      span_ [] (text_ (("user " <>) . showT <$> readRVar uid))
      button_ [onClick (modifyRVar uid (\i -> min 11 (i + 1)))] "+"
    p_ [] (text_ (showResult <$> name))

  -- With 'reactAsyncIOInto': the result goes into an 'RVar' of our own, so a handler can write it too.
  -- Saving a new name shows it immediately (an optimistic update) and then sends the request; the
  -- server's answer replaces it about 1.5 s later, and an error replaces it with the error.
  section_ [] $ do
    request <- newRVar (Load 1)
    name <- newRVar (ReactIOPending Nothing)
    reactAsyncIOInto name (readRVar request) runRequest
    newName <- newRVar ("" :: Text)
    let user = requestUser <$> readRVar request
        rename = do
          uid <- untracked user
          n <- Text.strip <$> readRVar newName
          unless (Text.null n) $ do
            writeRVar name (ReactIOSuccess n)
            writeRVar request (Rename uid n)
            writeRVar newName ""
    p_ [] "reactAsyncIOInto: rename the selected user, with an optimistic update."
    div_ [] $ do
      button_ [onClick (untracked user >>= writeRVar request . Load . max 1 . subtract 1)] "-"
      span_ [] (text_ (("user " <>) . showT <$> user))
      button_ [onClick (untracked user >>= writeRVar request . Load . min 11 . (+ 1))] "+"
      input_ [placeholder_ "New name", bindValue newName]
      button_ [onClick rename] "Rename"
    p_ [] (text_ (showResult <$> readRVar name))
  where
    showT = Text.pack . show

-- | The name of the user with the given id.
fetchUserName :: Int -> IO Text
fetchUserName uid = do
  name <- js_fetchUserName uid
  if isNull name then
    throwIO (userError ("could not load user " <> show uid))
  else
    pure (Text.pack (fromJSString name))

-- | Ask the server to rename the user, returning the new name it confirms.
renameUser :: Int -> Text -> IO Text
renameUser uid name = do
  confirmed <- js_renameUser uid (toJSString (Text.unpack name))
  if isNull confirmed then
    throwIO (userError ("could not rename user " <> show uid))
  else
    pure (Text.pack (fromJSString confirmed))

type Request :: Type
data Request
  = Load Int
  | Rename Int Text

runRequest :: Request -> IO Text
runRequest = \case
  Load uid -> fetchUserName uid
  Rename uid name -> renameUser uid name

requestUser :: Request -> Int
requestUser = \case
  Load uid -> uid
  Rename uid _ -> uid

foreign import javascript interruptible "((id, $c) => setTimeout(() => \
  \  fetch('https://jsonplaceholder.typicode.com/users/' + id) \
  \    .then(r => r.ok ? r.json() : Promise.reject(r.status)) \
  \    .then(u => $c(String(u.name)), () => $c(null)), 500))"
  js_fetchUserName :: Int -> IO JSVal

foreign import javascript interruptible "((id, name, $c) => setTimeout(() => \
  \  fetch('https://jsonplaceholder.typicode.com/users/' + id, { \
  \    method: 'PATCH', \
  \    headers: { 'Content-Type': 'application/json' }, \
  \    body: JSON.stringify({ name: name }) }) \
  \    .then(r => r.ok ? r.json() : Promise.reject(r.status)) \
  \    .then(u => $c(String(u.name)), () => $c(null)), 1500))"
  js_renameUser :: Int -> JSVal -> IO JSVal

showResult :: ReactIO Text -> Text
showResult = \case
  ReactIOPending Nothing -> "Loading…"
  -- The previous result, while the next one is loading.
  ReactIOPending (Just n) -> n <> " (loading…)"
  ReactIOSuccess n -> n
  ReactIOException Nothing e -> "Error: " <> showException e
  -- The last successful result, next to the error of the latest run.
  ReactIOException (Just n) e -> n <> " (error: " <> showException e <> ")"
  where
    -- 'displayException' on 'SomeException' itself would include the backtrace.
    showException (SomeException e) = Text.pack (displayException e)
