module Main where

import Brick
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Request

-- import Screen (problemsList)

ui :: Widget ()
ui =
  withBorderStyle unicode $
    borderWithLabel
      (str "Hello!")
      (center (str "LeftLeftLeftLeftLeftLeftLeftLeftLeftLeftLeftLeft\nLeftLeftLeftLeftLeftLeftLeftLeftLeftLeftLeftLeftLeftLeftLeftLeftLeftLeftLeft") <+> vBorder <+> center (str "Right"))

main :: IO ()
-- main = simpleMain problemsList
main = simpleMain ui

-- main = someFunc

-- someFunc
