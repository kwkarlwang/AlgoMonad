{-# LANGUAGE OverloadedStrings #-}

module Backend.UserInfo where

import Backend.Utils
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.List (intercalate)
import Network.HTTP.Req

type UserInfo = [(String, String)]

requestUserInfo :: (FromJSON a) => IO (Req (JsonResponse a))
requestUserInfo = do
  req POST (https "leetcode.com" /: "graphql") (ReqBodyJson payload) jsonResponse <$> getCredentials
  where
    payload =
      object
        [ "query"
            .= intercalate
              "\n"
              [ "{",
                "  user {",
                "    username",
                "    isCurrentUserPremium",
                "  }",
                "}"
              ]
        ]

getUserInfo :: IO UserInfo
getUserInfo = do
  request <- requestUserInfo
  reqData <- getResponseBody request
  return $ case reqData of
    Null -> do
      error "Please login in to leetcode in Chrome and try again"
    _ ->
      let username = reqData ^? key "user" . key "username"
          isCurrentUserPremium = reqData ^? key "user" . key "isCurrentUserPremium"
       in map
            ( \(key, maybe) ->
                case maybe of
                  Just (String x) -> (key, init . tail . show $ x)
                  Just (Bool x) -> (key, show x)
                  _ -> error "Need to re login in the browser"
            )
            [("username", username), ("premium", isCurrentUserPremium)]
