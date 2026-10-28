{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | The create-mode interpreter, used in the browser for the items of keyed lists.
--
-- Walks the same 'Html' as the other interpreters, but builds every node with DOM functions
-- (@createElement@, @createTextNode@, ...), and attaches listeners and effects to the nodes it
-- creates.
module FunPage.Internal.Create where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import FunPage.Internal.FFI (
  Element,
  appendChild,
  createElement,
  createTextNode,
  focus,
  insertNodeAt,
  onEvent,
  removeNode,
  setAttrBool,
  setAttrText,
  setAttribute,
  setDocumentTitle,
  setHtmlAttribute,
  setProperty,
 )
import FunPage.Internal.Html (
  Backend (Backend, backendEffect, backendElement, backendHead, backendKeyed, backendText, backendTextHole),
  ElementInfo (
    elementBoolAttributes,
    elementEvents,
    elementFocus,
    elementProperties,
    elementTag,
    elementTextAttributes
  ),
  HeadItem (HeadHtmlAttribute, HeadStylesheet, HeadTitle),
  Html,
  HtmlError (HtmlError),
  runHtml,
 )
import FunPage.Internal.Monad (Reactive, reactiveEffect, runHandler)
import FunPage.Internal.Reactive (Graph, onCleanup, withOwner)

-- | Where created nodes go.
type CreateTarget :: Type
data CreateTarget
  = -- | Appended to an element.
    CreateTargetAppended (IORef [Element])
  | -- | Collected as the roots of an item.
    CreateTargetRoots Element

mkBackend :: Graph -> IORef CreateTarget -> Backend
mkBackend graph target =
  Backend
    { backendText = \s -> createTextNode s >>= place
    , backendElement = \info children -> do
        el <- createElement (elementTag info)
        attachElement graph el info
        place el
        prev <- readIORef target
        writeIORef target (CreateTargetRoots el)
        children
        writeIORef target prev
    , backendTextHole = \r ->
        readIORef target >>= \case
          CreateTargetRoots parent ->
            reactiveEffect graph r (setAttrText parent "textContent")
          CreateTargetAppended _ ->
            throwIO (HtmlError "fun-page: text_ must be inside an element")
    , backendKeyed = \items key item ->
        readIORef target >>= \case
          CreateTargetRoots parent ->
            keyedList graph parent items key item
          CreateTargetAppended _ ->
            throwIO (HtmlError "fun-page: forEachKeyed must be inside an element")
    , backendHead = attachHead graph
    , backendEffect = reactiveEffect graph
    }
  where
    place node =
      readIORef target >>= \case
        CreateTargetRoots parent -> appendChild parent node
        CreateTargetAppended roots -> modifyIORef' roots (node :)

-- | Attach listeners and effects to an element (created or found by its key).
attachElement :: Graph -> Element -> ElementInfo -> IO ()
attachElement graph el info = do
  for_ (elementEvents info) $ \(ev, h) -> onEvent el ev (runHandler graph . h) >>= onCleanup graph
  for_ (elementTextAttributes info) $ \(k, v) -> reactiveEffect graph v (setAttribute el k . Just)
  for_ (elementBoolAttributes info) $ \(k, v) -> reactiveEffect graph v (setAttrBool el k)
  for_ (elementProperties info) $ \(k, v) -> reactiveEffect graph v (setProperty el k)
  for_ (elementFocus info) $ \r -> do
    prev <- newIORef False
    reactiveEffect graph r $ \b -> do
      was <- readIORef prev
      writeIORef prev b
      when (b && not was) (focus el)

-- | Head items in the browser. Stylesheets are already in the document.
attachHead :: Graph -> HeadItem -> IO ()
attachHead graph = \case
  HeadTitle r -> reactiveEffect graph r setDocumentTitle
  HeadStylesheet _ -> pure ()
  HeadHtmlAttribute k v -> reactiveEffect graph v (setHtmlAttribute k)

-- | Create the single root element of a keyed-list item.
createItem :: Graph -> Html () -> IO Element
createItem graph html = do
  roots <- newIORef []
  target <- newIORef (CreateTargetAppended roots)
  runHtml (mkBackend graph target) graph html
  readIORef roots >>= \case
    [el] -> pure el
    els ->
      throwIO . HtmlError $
        "fun-page: an item of forEachKeyed must be exactly one element, got "
          <> Text.pack (show (length els))
          <> " nodes"

-- | A live item: its root element and how to dispose it.
type Item :: Type
data Item = Item Element (IO ())

-- | Keep the children of the parent in sync with the list, one item per key.
keyedList :: (Ord k) => Graph -> Element -> Reactive [a] -> (a -> k) -> (a -> Html ()) -> IO ()
keyedList graph parent items key item = do
  setAttrText parent "textContent" ""
  live <- newIORef Map.empty
  onCleanup graph $ do
    old <- readIORef live
    for_ old $ \(Item el dispose) -> dispose *> removeNode el
    writeIORef live Map.empty
  reactiveEffect graph items $ \xs -> do
    old <- readIORef live
    let fresh = nubOn key xs
    new <- fmap Map.fromList . for fresh $ \(k, x) -> case Map.lookup k old of
      Just it -> pure (k, it)
      Nothing -> do
        (el, dispose) <- withOwner graph (createItem graph (item x))
        pure (k, Item el dispose)
    for_ (Map.difference old new) $ \(Item el dispose) -> dispose *> removeNode el
    for_ (zip [0 ..] fresh) $ \(i, (k, _)) ->
      for_ (Map.lookup k new) $ \(Item el _) -> insertNodeAt parent el i
    writeIORef live new
  where
    for = flip traverse

-- | Keep the first occurrence of every key, in order.
nubOn :: (Ord k) => (a -> k) -> [a] -> [(k, a)]
nubOn key = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member k seen = go seen xs
      | otherwise = (k, x) : go (Set.insert k seen) xs
      where
        k = key x
