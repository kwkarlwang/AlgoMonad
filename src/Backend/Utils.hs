{-# LANGUAGE OverloadedStrings #-}

module Backend.Utils where

import Backend.Cookie (getConfigFromFile)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Scientific (toRealFloat)
import qualified Data.Text as T
import Network.HTTP.Req
import System.Directory (getHomeDirectory)

langExtensionPair =
  [ ("c", ".c"),
    ("cpp", ".cpp"),
    ("csharp", ".cs"),
    ("golang", ".go"),
    ("java", ".java"),
    ("javascript", ".js"),
    ("typescript", ".ts"),
    ("kotlin", ".kt"),
    ("php", ".php"),
    ("python", ".py"),
    ("python3", ".py"),
    ("ruby", ".rb"),
    ("rust", ".rs"),
    ("scala", ".scala"),
    ("swift", ".swift"),
    ("racket", ".rkt"),
    ("erlang", ".erl"),
    ("elixir", ".exs")
  ]

langToExtension :: String -> String
langToExtension lang = snd $ head $ filter (\tup -> fst tup == lang) langExtensionPair

extensionToLang :: String -> String
extensionToLang ext = fst $ head $ filter (\tup -> snd tup == ext) langExtensionPair

getCredentials :: IO (Option scheme)
getCredentials = do
  (csrfToken, leetcodeSession) <- getConfigFromFile
  return $ (header "X-CSRFToken" csrfToken <>) <$> header "Cookie" $ "csrftoken=" <> csrfToken <> ";LEETCODE_SESSION=" <> leetcodeSession <> ";"

getResponseBody :: Req (JsonResponse Value) -> IO Value
getResponseBody request = do
  r <- runReq defaultHttpConfig request
  let res = responseBody r :: Value
  maybeToIO $ res ^? key "data"

maybeToIO :: Maybe Value -> IO Value
maybeToIO Nothing = return Null
maybeToIO (Just x) = return x

unpackString val = case val of
  Just (String val) -> T.unpack val
  _ -> ""

unpackInteger val = case val of
  Just (Integer val) -> val
  _ -> 0

unpackFloat val = case val of
  Just (Number val) -> toRealFloat val
  _ -> 0
