{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Main (main) where

import Control.Exception (try)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))

import FunPage.Html
import FunPage.JS
import FunPage.Reactive
import FunPage.Test

main :: IO ()
main =
  defaultMain . testGroup "fun-page" $
    [ testCase "counter" . testHtml counter $ do
        inc <- getElementById "inc"
        count <- getElementById "count"
        count0 <- getTextContent count
        assertEq count0 "0"
        click inc
        count1 <- getTextContent count
        assertEq count1 "1"
    , testCase "hidden attribute follows the state" . testHtml counter $ do
        reset <- getElementById "reset"
        getAttribute reset "hidden" >>= assertEq (Just "")
        getElementById "inc" >>= click
        getAttribute reset "hidden" >>= assertEq Nothing
        click reset
        getElementById "count" >>= getTextContent >>= assertEq "0"
        getAttribute reset "hidden" >>= assertEq (Just "")
    , testCase "every test starts from a fresh page" . testHtml counter $ do
        getElementById "count" >>= getTextContent >>= assertEq "0"
    , testCase "text input binding" . testHtml greeting $ do
        name <- getElementById "name"
        getElementById "greeting" >>= getTextContent >>= assertEq "Hello, !"
        setValue name "Ann"
        getElementById "greeting" >>= getTextContent >>= assertEq "Hello, Ann!"
        getElementById "clear" >>= click
        getValue name >>= assertEq ""
    , testCase "checkbox" . testHtml checkbox $ do
        box <- getElementById "box"
        isChecked box >>= assertEq False
        click box
        isChecked box >>= assertEq True
        getElementById "state" >>= getTextContent >>= assertEq "on"
    , testCase "keyed list" . testHtml list $ do
        new <- getElementById "new"
        let items = querySelectorAll "#items li" >>= traverse getTextContent
        items >>= assertEq ["a"]
        setValue new "b"
        keyDown new "Enter"
        setValue new "c"
        keyDown new "x"
        keyDown new "Enter"
        items >>= assertEq ["a", "b", "c"]
        getValue new >>= assertEq ""
        getElementById "drop" >>= click
        items >>= assertEq ["b", "c"]
    , testCase "jsEval" $ do
        jsEval "1 + 2" >>= (@?= "3")
        r <- try (jsEval "null.x")
        either (\(JSError _) -> True) (const False) r @? "jsEval throws"
    , testCase "jsAwait" $ do
        jsAwait "new Promise((ok) => setTimeout(() => ok('done'), 10))" >>= (@?= "done")
        jsAwait "42" >>= (@?= "42")
        r <- try (jsAwait "Promise.reject(new Error('no'))")
        either (\(JSError e) -> e == "Error: no") (const False) r @? "jsAwait throws"
    ]

counter :: Html ()
counter = do
  count <- newRVar (0 :: Int)
  div_ [] $ do
    button_ [id_ "dec", onClick (modifyRVar count (subtract 1))] "-"
    span_ [id_ "count"] (text_ (Text.pack . show <$> readRVar count))
    button_ [id_ "inc", onClick (modifyRVar count (+ 1))] "+"
    button_ [id_ "reset", onClick (writeRVar count 0), hidden ((== 0) <$> readRVar count)] "reset"

greeting :: Html ()
greeting = do
  name <- newRVar ("" :: Text)
  input_ [id_ "name", bindValue name]
  button_ [id_ "clear", onClick (writeRVar name "")] "clear"
  p_ [id_ "greeting"] (text_ ((\n -> "Hello, " <> n <> "!") <$> readRVar name))

checkbox :: Html ()
checkbox = do
  on <- newRVar False
  input_ [id_ "box", type_ "checkbox", checkedWith (readRVar on) (writeRVar on)]
  span_ [id_ "state"] (text_ ((\b -> if b then "on" else "off") <$> readRVar on))

list :: Html ()
list = do
  items <- newRVar ["a" :: Text]
  new <- newRVar ""
  let add ev
        | eventKey ev == "Enter" = do
            t <- readRVar new
            modifyRVar items (<> [t])
            writeRVar new ""
        | otherwise = pure ()
  input_ [id_ "new", bindValue new, onKeyDown add]
  button_ [id_ "drop", onClick (modifyRVar items (drop 1))] "drop first"
  ul_ [id_ "items"] (forEachKeyed (readRVar items) id (\t -> li_ [] (text_ (pure t))))
