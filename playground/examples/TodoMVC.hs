{-# LANGUAGE GHC2021, OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Main (main) where

import Control.Monad (filterM, join, unless, when)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text

import FunPage.Html
import FunPage.Reactive
import FunPage.Site (mountPage)

-- | A todo item. The fields that change are variables of their own, so changing them does not change the list.
type Task :: Type
data Task = Task
  { taskId :: Int
  , taskTitle :: RVar Text
  , taskCompleted :: RVar Bool
  }

-- | Which tasks are shown.
type Filter :: Type
data Filter = All | Active | Completed
  deriving stock (Eq)

matches :: (MonadReactiveRead m) => Filter -> Task -> m Bool
matches All _ = pure True
matches Active t = not <$> readRVar (taskCompleted t)
matches Completed t = readRVar (taskCompleted t)

-- | The tasks matching a filter.
tasksMatching :: (MonadReactiveRead m) => Filter -> [Task] -> m [Task]
tasksMatching f = filterM (matches f)

-- | TodoMVC, following the markup of https://github.com/tastejs/todomvc-app-template.
todoMvc :: Html ()
todoMvc = do
  htmlAttribute "lang" "en"
  title "fun-page • TodoMVC"
  stylesheet "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.css"
  stylesheet "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.4.3/index.css"
  tasks <- newRVar ([] :: [Task])
  nextId <- newRVar (0 :: Int)
  newTitle <- newRVar ""
  currentFilter <- newRVar All
  let noTasks = null <$> readRVar tasks
      countMatching f = length <$> (readRVar tasks >>= tasksMatching f)
      activeCount = countMatching Active
      completedCount = countMatching Completed
  section_ [class_ "todoapp"] $ do
    header_ [class_ "header"] $ do
      h1_ [] "todos"
      input_
        [ class_ "new-todo"
        , placeholder_ "What needs to be done?"
        , autofocus
        , bindValue newTitle
        , onKeyDown $ \ev ->
            when (eventKey ev == "Enter") (addTask tasks nextId newTitle)
        ]
    section_ [class_ "main", hidden noTasks] $ do
      input_
        [ id_ "toggle-all"
        , class_ "toggle-all"
        , type_ "checkbox"
        , checkedWith ((== 0) <$> activeCount) $ \b ->
            readRVar tasks >>= traverse_ (\t -> writeRVar (taskCompleted t) b)
        ]
      label_ [for_ "toggle-all"] "Mark all as complete"
      ul_ [class_ "todo-list"] $
        forEachKeyed
          (join (tasksMatching <$> readRVar currentFilter <*> readRVar tasks))
          taskId
          (\t -> taskItem (modifyRVar tasks (filter ((/= taskId t) . taskId))) t)
    footer_ [class_ "footer", hidden noTasks] $ do
      span_ [class_ "todo-count"] $ do
        strong_ [] (text_ (Text.pack . show <$> activeCount))
        span_ [] (text_ ((\n -> if n == 1 then " item left" else " items left") <$> activeCount))
      ul_ [class_ "filters"] $ do
        filterLink currentFilter All "#/" "All"
        filterLink currentFilter Active "#/active" "Active"
        filterLink currentFilter Completed "#/completed" "Completed"
      button_
        [ class_ "clear-completed"
        , hidden ((== 0) <$> completedCount)
        , onClick (readRVar tasks >>= tasksMatching Active >>= writeRVar tasks)
        ]
        "Clear completed"
  footer_ [class_ "info"] $ do
    p_ [] "Double-click to edit a todo"
    p_ [] $ do
      "Created with "
      a_ [href_ "https://github.com/t4ccer/fun-page"] "fun-page"

-- | A filter link. Keeps the template's hash links; nothing reads the hash yet.
filterLink :: RVar Filter -> Filter -> Text -> Html () -> Html ()
filterLink current f href label =
  li_ [] $
    a_
      [href_ href, classWhen [("selected", (== f) <$> readRVar current)], onClick (writeRVar current f)]
      label

addTask :: RVar [Task] -> RVar Int -> RVar Text -> Handler ()
addTask tasks nextId newTitle = do
  title <- Text.strip <$> readRVar newTitle
  unless (Text.null title) $ do
    taskId <- readRVar nextId
    writeRVar nextId (taskId + 1)
    taskTitle <- newRVar title
    taskCompleted <- newRVar False
    modifyRVar tasks (<> [Task taskId taskTitle taskCompleted])
    writeRVar newTitle ""

-- | One task. Gets the task's own variables and how to remove it, not the whole list.
taskItem :: Handler () -> Task -> Html ()
taskItem remove task = do
  editing <- newRVar False
  editText <- newRVar ""

  let startEditing = do
        readRVar (taskTitle task) >>= writeRVar editText
        writeRVar editing True

      save = do
        isEditing <- readRVar editing
        when isEditing $ do
          writeRVar editing False
          newTitle <- Text.strip <$> readRVar editText
          if Text.null newTitle then remove else writeRVar (taskTitle task) newTitle

      cancel = writeRVar editing False

  li_ [classWhen [("completed", readRVar (taskCompleted task)), ("editing", readRVar editing)]] $ do
    div_ [class_ "view"] $ do
      input_ [class_ "toggle", type_ "checkbox", bindChecked (taskCompleted task)]
      label_ [onDoubleClick startEditing] (text_ (readRVar (taskTitle task)))
      button_ [class_ "destroy", onClick remove] ""
    input_
      [ class_ "edit"
      , bindValue editText
      , focusWhen (readRVar editing)
      , onBlur save
      , onKeyDown $ \ev ->
          case eventKey ev of
            "Enter" -> save
            "Escape" -> cancel
            _ -> pure ()
      ]

main :: IO ()
main = mountPage "app" todoMvc
