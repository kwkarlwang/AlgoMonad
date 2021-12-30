{-# LANGUAGE OverloadedStrings #-}

module Frontend.Utils where

import Brick (Widget (Widget), str, withAttr)
import Brick.Util (fg, on)
import Brick.Widgets.Border (borderAttr)
import Brick.Widgets.List (list)
import qualified Data.Function as F (on)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Graphics.Vty.Attributes

colorMap =
  [ ("white", fg brightWhite),
    ("selected" <> "black", withStyle (black `on` magenta) bold),
    ("red", fg red),
    ("selected" <> "red", withStyle (black `on` magenta) bold),
    ("yellow", fg yellow),
    ("selected" <> "yellow", withStyle (black `on` magenta) bold),
    ("green", fg green),
    ("selected" <> "green", withStyle (black `on` magenta) bold),
    ("tab", black `on` brightBlack),
    ("selected" <> "tab", withStyle (black `on` brightWhite) bold),
    (borderAttr, fg brightWhite)
  ]

drawStr :: Bool -> String -> Widget n
drawStr b = (if b then withAttr ("selected" <> "black") else withAttr "white") . str

drawRed :: Bool -> String -> Widget n
drawRed b = (if b then withAttr ("selected" <> "red") else withAttr "red") . str

drawGreen :: Bool -> String -> Widget n
drawGreen b = (if b then withAttr ("selected" <> "green") else withAttr "green") . str

drawYellow :: Bool -> String -> Widget n
drawYellow b = (if b then withAttr ("selected" <> "yellow") else withAttr "yellow") . str

drawTab :: Bool -> String -> Widget n
drawTab b = (if b then withAttr ("selected" <> "tab") else withAttr "tab") . str

floatDiv :: Integer -> Integer -> Float
floatDiv = (/) `F.on` fromIntegral

floatRound :: Float -> Int -> Float
floatRound num numDigit = floatDiv (round (num * multFloat)) multInt
  where
    multInt = 10 ^ numDigit :: Integer
    multFloat = 10 ^ numDigit :: Float
