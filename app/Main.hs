module Main where

import Backend.Cookie (chromeMacPath, getChromeCookie)
import Frontend.App (tui)
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
