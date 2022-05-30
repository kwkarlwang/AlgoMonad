module Main where

import App (tui)
import Backend.Cookie (chromeMacPath, getChromeCookie)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["login", userProfilePath] -> do
      let cookiePath = userProfilePath ++ "/Cookies"
      getChromeCookie cookiePath
      print "Cookie acquired successfully"
    ["login"] -> do
      cookiePath <- chromeMacPath
      getChromeCookie cookiePath
      print "Cookie acquired successfully"
    _ -> tui
