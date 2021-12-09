module Frontend.Help where

import Brick (Widget, hBox, padLeftRight, vBox)
import Frontend.State (ResourceName)
import Frontend.Utils (drawGreen, drawStr, drawYellow)

renderHelp :: Widget ResourceName
renderHelp = padLeftRight 2 $ hBox [vBox $ map (\tup -> drawYellow False $ fst tup ++ " ") help, vBox $ map (drawStr False . snd) help]

help =
  [ ("h", "go back"),
    ("j", "move down"),
    ("k", "move up"),
    ("l", "select question"),
    ("enter", "download question"),
    ("/", "search by title"),
    ("=", "search by id"),
    ("g", "move to top"),
    ("G", "move to bottom"),
    ("ctrl-d", "move half page down"),
    ("ctrl-u", "move half page up"),
    ("q", "quit app")
  ]
