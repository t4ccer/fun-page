# `fun-page`

`fun-page` is a simple, functional, reactive web framework that makes frontend fun.

## Sample

```haskell
counter :: Html ()
counter = do
  count <- newRVar (0 :: Int)
  div_ [] $ do
    button_ [onClick (modifyRVar count (subtract 1))] "-"
    span_ [] (text_ (Text.pack . show <$> readRVar count))
    button_ [onClick (modifyRVar count (+ 1))] "+"
    button_ [onClick (writeRVar count 0), hidden ((== 0) <$> readRVar count)] "reset"
```

See other [examples](./examples) and the web [playground](https://t4ccer.com/fun-page/playground/).

## Prerequisites

- GHC with JS backend (`javascript-unknown-ghcjs-ghc`)
- `nodejs`

## Overview

A page is an expression of type `Html ()`.
Inside `Html` monad you can create HTML elements and create reactive variables (`RVar`).
Most elements and attributes take arguments of types `Reactive Text` and `Handler ()`. 
`Reactive` is a monad which allows only reading `RVar`s and automatically re-runs if any of the variables change so e.g. `text_ (readRVar message)` will update the text node whenever `message` changes.
`Handler` allows both read and write access to `RVar`s and is used for event handlers.

```haskell
span_ [] (text_ (Text.pack . show <$> readRVar count))
--              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--              |
--              +-- Reactive Text


button_ [onClick (modifyRVar count (+ 1))] "+"
--               ^^^^^^^^^^^^^^^^^^^^^^^^
--               |
--               +-- Handler ()
```

| Action/Monad            |               `Html`                | `Reactive` | `Handler` |
|:------------------------|:-----------------------------------:|:----------:|:---------:|
| `newRVar`               |                 ✅                  |     ❌     |    ✅     |
| `readRVar`              | ❌<sup>[1](#dynamic-elements)</sup> |     ✅     |    ✅     |
| `writeRVar`             |                 ❌                  |     ❌     |    ✅     |
| `div_`, `button_`, etc. |                 ✅                  |     ❌     |    ❌     |

`fun-page` compiles to static files so you can serve your webpage using e.g. github pages and does not require a Haskell server to run.
Declare `.html` files with
```haskell
main :: IO ()
main = runSite $ emptySite {sitePages = [page "index.html" counter]}
```
Then `cabal run` will create `.fun-page-out` directory with your `.html` and `.js` files.

There is no virtual DOM, every `Reactive` value knows on which variables it depends and can update
only that specific node or attribute without recreating DOM elements or diffing.

There are no components, extract reusable parts of your page into top-level `Html ()` functions.
Then by doing `do counter; counter` you get two independent counters with no need for manual state or message threading.

There are no messages. The only way of communicating between parts of the webpage is by writing to `RVar`s.

A simple API ideal for humans.

### `IO`

`Handler` also allows to run `IO` actions but one need to be careful to not block the thread as the callback would then throw.

If you know that your `IO` action is synchronous then `handlerPerformSyncIO :: IO a -> Handler a` is all you need.

There are a few ways of doing async `IO`.
The simplest way is with `writeRVarAsyncIO :: RVar a -> IO a -> Handler ()`. It will run the `IO` action in the background and write the result to the `RVar` when it finishes.
The value goes back to the reactive system so everybody who depends on that value will be notified once the `IO` finishes.

Another way of doing async `IO` is by transforming a `Reactive` value with `reactAsyncIO :: Reactive a -> (a -> IO b) -> Html (Reactive (ReactIO b))`.
It will start an async `IO` action whenever the passed `Reactive` value changes and will produce a new `Reactive` with the value of the finished `IO` action.
`ReactIO` tells us if the `IO` finished, is pending, or threw an exception.

Async `IO` functions and `RVar`s handle multiple writes and concurrent `IO` by default.
Last write (by start time, not `IO` finish time) wins (e.g. if you fetch data on a button click and user clicks the button multiple times then only the data from the last click is visible no matter in what order the requests finish).

### Dynamic Elements

i.e. "how do I read an `RVar` in `Html`?" (you don't).

Say you have an `RVar [Task]` and want to create some HTML for each of the tasks with `showTask :: Task -> Html ()`.
Haskell intuition says `traverse` but that's not the answer here, because you cannot read an `RVar` in `Html`.
But `Html` only allows to set up the DOM so it wouldn't be able to react as the list changes, the answer is
```haskell
forEachKeyed :: (Foldable f, Ord k) => Reactive (f a) -> (a -> k) -> (a -> Html ()) -> Html ()
```
It allows to create an `Html` element for each element of some `Foldable` that's also `Reactive` so it will keep track of changes over time.
The second argument is the key function that associates your input elements with the DOM elements.
If the key does not change the passed `a -> Html ()` will not re-run so use nested `RVar`s if both the list and the elements may change (removing task from the list and adding a new one with the same id but different title wouldn't update the DOM).
If the key changes place in the sequence it will only move the DOM element and if the key is removed from the sequence it will remove the DOM element.
Now putting everything together:
```haskell
data Task = Task
  { taskId :: Int
  , taskTitle :: RVar Text
  }

taskList :: Html ()
taskList = do
  tasks <- newRVar [...]
  ul_ [] (forEachKeyed (readRVar tasks) taskId showTask)

showTask :: Task -> Html ()
showTask task = do
  li_ [] $ do
    span_ [] $ text_ (readRVar $ taskName task)
```

See how we do it in [our TodoMVC implementation](./examples/TodoMVC.hs).

## GHC JS Backend Oddities

Problem:
```
<command line>: cannot satisfy -package-id base-<...>
```
Run `cabal clean`. That tends to happen after every modification of the cabal files.

Problem:
```
Failed to load dynamic interface file for Prelude:
  <...>/Prelude.dyn_hi: does not exist
```
Run `cabal haddock --project-file=cabal.project.native --builddir=dist-native`

## License

Copyright (C) 2026 Tomasz Maciosowski (t4ccer)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
