{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Utils where

import Brick (Widget, str, withAttr)
import Brick.Util (fg, on)
import Brick.Widgets.Border (borderAttr)
import qualified Data.Function as F (on)
import Graphics.Vty.Attributes

colorMap =
  [ ("white", fg brightWhite),
    ("bold" <> "white", withStyle (fg brightWhite) bold),
    ("selected" <> "black", withStyle (black `on` magenta) bold),
    ("red", fg red),
    ("selected" <> "red", withStyle (black `on` magenta) bold),
    ("bold" <> "red", withStyle (fg red) bold),
    ("yellow", fg yellow),
    ("selected" <> "yellow", withStyle (black `on` magenta) bold),
    ("green", fg green),
    ("selected" <> "green", withStyle (black `on` magenta) bold),
    ("bold" <> "green", withStyle (fg green) bold),
    ("tab", black `on` brightBlack),
    ("selected" <> "tab", withStyle (black `on` brightWhite) bold),
    ("bold" <> "cyan", withStyle (fg cyan) bold),
    (borderAttr, fg brightWhite)
  ]

drawStr :: Bool -> String -> Widget n
drawStr b = (if b then withAttr ("selected" <> "black") else withAttr "white") . str

drawBoldStr :: String -> Widget n
drawBoldStr = withAttr ("bold" <> "white") . str

drawRed :: Bool -> String -> Widget n
drawRed b = (if b then withAttr ("selected" <> "red") else withAttr "red") . str

drawBoldRed :: String -> Widget n
drawBoldRed = withAttr ("bold" <> "red") . str

drawGreen :: Bool -> String -> Widget n
drawGreen b = (if b then withAttr ("selected" <> "green") else withAttr "green") . str

drawBoldGreen :: String -> Widget n
drawBoldGreen = withAttr ("bold" <> "green") . str

drawYellow :: Bool -> String -> Widget n
drawYellow b = (if b then withAttr ("selected" <> "yellow") else withAttr "yellow") . str

drawTab :: Bool -> String -> Widget n
drawTab b = (if b then withAttr ("selected" <> "tab") else withAttr "tab") . str

drawBoldCyan :: String -> Widget n
drawBoldCyan = withAttr ("bold" <> "cyan") . str

floatDiv :: Integer -> Integer -> Float
floatDiv = (/) `F.on` fromIntegral

floatRound :: Float -> Int -> Float
floatRound num numDigit = floatDiv (round (num * multFloat)) multInt
  where
    multInt = 10 ^ numDigit :: Integer
    multFloat = 10 ^ numDigit :: Float
