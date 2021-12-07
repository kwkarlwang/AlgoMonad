{-# LANGUAGE OverloadedStrings #-}

module Frontend.Utils where

import Brick (Widget (Widget), str, withAttr)
import Brick.Widgets.List (list)
import Cursor.Simple.List.NonEmpty (NonEmptyCursor, makeNonEmptyCursor)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

drawStr :: Bool -> String -> Widget n
drawStr b = (if b then withAttr ("selected" <> "black") else withAttr "white") . str

drawRed :: Bool -> String -> Widget n
drawRed b = (if b then withAttr ("selected" <> "red") else withAttr "red") . str

drawGreen :: Bool -> String -> Widget n
drawGreen b = (if b then withAttr ("selected" <> "green") else withAttr "green") . str

drawYellow :: Bool -> String -> Widget n
drawYellow b = (if b then withAttr ("selected" <> "yellow") else withAttr "yellow") . str

floatDiv :: Integer -> Integer -> Float
floatDiv = (/) `on` fromIntegral

floatRound :: Float -> Int -> Float
floatRound num numDigit = floatDiv (round (num * multFloat)) multInt
  where
    multInt = 10 ^ numDigit :: Integer
    multFloat = 10 ^ numDigit :: Float
