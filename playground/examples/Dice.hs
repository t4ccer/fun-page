{-# LANGUAGE GHC2021, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE JavaScriptFFI #-}

-- | Rolling a die with 'modifyRVarAsyncIO'.
module Main (main) where

import Data.Kind (Type)
import Data.Text qualified as Text

import FunPage.Html (Html, button_, div_, onClick, p_, span_, text_)
import FunPage.JS (jsAwait)
import FunPage.Reactive (modifyRVarAsyncIO, newRVar, readRVar, writeRVar)
import FunPage.Site (mountPage)

-- | The last roll, if any, and the sum of all rolls so far.
type Dice :: Type
data Dice = Dice
  { diceLast :: Maybe Int
  , diceTotal :: Int
  }

noRolls :: Dice
noRolls = Dice {diceLast = Nothing, diceTotal = 0}

addRoll :: Dice -> Int -> Dice
addRoll d r = Dice {diceLast = Just r, diceTotal = diceTotal d + r}

-- | Roll a six-sided die. Imagine this is generated on a remote server.
rollDie :: IO Int
rollDie = read . Text.unpack <$> jsAwait "(1 + Math.floor(Math.random() * 6))"

dice :: Html ()
dice = do
  state <- newRVar noRolls
  div_ [] $ do
    button_ [onClick (modifyRVarAsyncIO state rollDie addRoll)] "Roll"
    button_ [onClick (writeRVar state noRolls)] "Reset"
  p_ [] $ do
    "Roll: "
    span_ [] (text_ (maybe "-" (Text.pack . show) . diceLast <$> readRVar state))
  p_ [] $ do
    "Total: "
    span_ [] (text_ (Text.pack . show . diceTotal <$> readRVar state))

main :: IO ()
main = mountPage "app" dice
