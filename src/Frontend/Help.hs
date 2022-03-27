module Frontend.Help where

import Brick (Widget, hBox, padLeftRight, vBox)
import Frontend.State (ResourceName)
import Frontend.Utils (drawGreen, drawRed, drawStr, drawYellow)

renderHelp :: Widget ResourceName
renderHelp = padLeftRight 2 $ hBox [vBox $ map (\tup -> drawYellow False $ fst tup ++ " ") help, vBox $ map (drawStr False . snd) help]

help =
  [ ("Pro tip", "select language with SHIFT+"),
    ("", "prefix of the language"),
    (" ", " "),
    ("1/2", "switch tab"),
    ("h", "go back"),
    ("j", "move down"),
    ("k", "move up"),
    ("l", "select question"),
    ("enter", "download question"),
    ("/", "search by title"),
    ("=", "search by id"),
    ("n", "search next occurance"),
    ("g", "move to top"),
    ("G", "move to bottom"),
    ("ctrl-d", "move half page down"),
    ("ctrl-u", "move half page up"),
    ("q", "quit app")
  ]
