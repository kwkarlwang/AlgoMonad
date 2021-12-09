module Main where

import Backend.Cookie (getChromeCookie)
import Frontend.App (tui)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["login"] -> do
      getChromeCookie
      print "Cookie acquired successfully"
    _ -> tui
