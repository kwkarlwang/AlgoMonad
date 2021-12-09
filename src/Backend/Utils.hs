{-# LANGUAGE OverloadedStrings #-}

module Backend.Utils where

import Backend.Cookie (getConfigFromFile)
import Control.Lens hiding ((.=))
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Network.HTTP.Req
import System.Directory (getHomeDirectory)

getCredentials :: IO (Option scheme)
getCredentials = do
  (csrfToken, leetcodeSession) <- getConfigFromFile False
  return $ header "Cookie" $ "csrftoken=" <> csrfToken <> ";LEETCODE_SESSION=" <> leetcodeSession <> ";"

getResponseBody :: Req (JsonResponse Value) -> IO Value
getResponseBody request = do
  r <- runReq defaultHttpConfig request
  let res = responseBody r :: Value
  maybeToIO $ res ^? key "data"

maybeToIO :: Maybe Value -> IO Value
maybeToIO Nothing = return Null
maybeToIO (Just x) = return x
