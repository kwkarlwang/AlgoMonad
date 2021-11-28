{-# LANGUAGE OverloadedStrings #-}

module Backend.UserInfo where

import Backend.Utils
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.List (intercalate)
import Network.HTTP.Req

type UserInfo = [(String, String)]

requestUserInfo :: (FromJSON a) => Req (JsonResponse a)
requestUserInfo =
  req POST (https "leetcode.com" /: "graphql") (ReqBodyJson payload) jsonResponse headers
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
    -- headersMap = [("Referer", "https://leetcode.com"), ("Origin", "https://leetcode.com")]
    -- headers = makeHeaders headersMap <> getCredentials
    headers = getCredentials

getUserInfo :: IO UserInfo
getUserInfo = do
  reqData <- getResponseBody requestUserInfo
  return $ case reqData of
    Null -> error "Need to re login in the browser"
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
