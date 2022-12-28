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
    ("unfocus" <> "selected" <> "black", withStyle (black `on` white) bold),
    ("red", fg red),
    ("selected" <> "red", withStyle (black `on` magenta) bold),
    ("bold" <> "red", withStyle (fg red) bold),
    ("yellow", fg yellow),
    ("selected" <> "yellow", withStyle (black `on` magenta) bold),
    ("bold" <> "yellow", withStyle (fg yellow) bold),
    ("green", fg green),
    ("selected" <> "green", withStyle (black `on` magenta) bold),
    ("bold" <> "green", withStyle (fg green) bold),
    ("tab", black `on` brightBlack),
    ("selected" <> "tab", withStyle (black `on` brightWhite) bold),
    ("cyan", fg cyan),
    ("selected" <> "cyan", withStyle (black `on` cyan) bold),
    ("bold" <> "cyan", withStyle (fg cyan) bold),
    (borderAttr, fg brightWhite)
  ]

drawStr :: String -> Widget n
drawStr = withAttr "white" . str

drawSelectedStr :: String -> Widget n
drawSelectedStr = withAttr ("selected" <> "black") . str

drawUnfocusStr :: String -> Widget n
drawUnfocusStr = withAttr ("unfocus" <> "selected" <> "black") . str

drawBoldStr :: String -> Widget n
drawBoldStr = withAttr ("bold" <> "white") . str

drawRed :: String -> Widget n
drawRed = withAttr "red" . str

drawBoldRed :: String -> Widget n
drawBoldRed = withAttr ("bold" <> "red") . str

drawGreen :: String -> Widget n
drawGreen = withAttr "green" . str

drawBoldGreen :: String -> Widget n
drawBoldGreen = withAttr ("bold" <> "green") . str

drawYellow :: String -> Widget n
drawYellow = withAttr "yellow" . str

drawBoldYellow :: String -> Widget n
drawBoldYellow = withAttr ("bold" <> "yellow") . str

drawTab :: Bool -> String -> Widget n
drawTab b = (if b then withAttr ("selected" <> "tab") else withAttr "tab") . str

drawCyan :: Bool -> String -> Widget n
drawCyan b = (if b then withAttr ("selected" <> "cyan") else withAttr "cyan") . str

drawBoldCyan :: String -> Widget n
drawBoldCyan = withAttr ("bold" <> "cyan") . str

drawSelected :: Bool -> (String -> Widget n) -> (String -> Widget n)
drawSelected True def = drawSelectedStr
drawSelected False def = def

--                  isSelected  hasFocus
drawSelectedOrUnfocus :: Bool -> Bool -> (String -> Widget n) -> (String -> Widget n)
drawSelectedOrUnfocus True True def = drawSelectedStr
drawSelectedOrUnfocus True False def = drawUnfocusStr
drawSelectedOrUnfocus _ _ def = def

floatDiv :: Integer -> Integer -> Float
floatDiv = (/) `F.on` fromIntegral

floatRound :: Float -> Int -> Float
floatRound num numDigit = floatDiv (round (num * multFloat)) multInt
  where
    multInt = 10 ^ numDigit :: Integer
    multFloat = 10 ^ numDigit :: Float
