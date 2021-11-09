module Main where

import Brick
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Request
import Screen (tui)

main :: IO ()
main = tui
