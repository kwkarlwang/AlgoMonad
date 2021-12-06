{-# LANGUAGE OverloadedStrings #-}

module Frontend.Utils where

import Brick (Widget (Widget), str, withAttr)
import Brick.Widgets.List (list)
import Cursor.Simple.List.NonEmpty (NonEmptyCursor, makeNonEmptyCursor)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

drawStr :: Bool -> String -> Widget n
drawStr b = (if b then withAttr "selected" else id) . str

floatDiv :: Integer -> Integer -> Float
floatDiv = (/) `on` fromIntegral

floatRound :: Float -> Int -> Float
floatRound num numDigit = floatDiv (round (num * multFloat)) multInt
  where
    multInt = 10 ^ numDigit :: Integer
    multFloat = 10 ^ numDigit :: Float
