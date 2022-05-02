{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UserInfo.Request where

import Backend.Utils
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Req
import UserInfo.State

requestUserInfo :: (FromJSON a) => IO (Req (JsonResponse a))
requestUserInfo = req POST (https "leetcode.com" /: "graphql") (ReqBodyJson payload) jsonResponse <$> getCredentials
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

requestUserQuestionCount :: (FromJSON a) => String -> IO (Req (JsonResponse a))
requestUserQuestionCount username = req POST (https "leetcode.com" /: "graphql") (ReqBodyJson payload) jsonResponse <$> getCredentials
  where
    payload =
      object
        [ "query"
            .= intercalate
              "\n"
              [ "{",
                "  allQuestionsCount {",
                "    difficulty",
                "    count",
                "  }",
                "  matchedUser(username: \"" ++ username ++ "\") {",
                "    submitStatsGlobal {",
                "      acSubmissionNum {",
                "        difficulty",
                "        count",
                "      }",
                "    }",
                "  }",
                "}"
              ]
        ]

extractQuestionCount :: (Value, Value) -> Maybe QuestionCount
extractQuestionCount (allQuestionCountJson, solvedQuestionCountJson) = do
  difficulty <- fmap T.unpack $ allQuestionCountJson ^? key "difficulty" . _String
  count <- allQuestionCountJson ^? key "count" . _Integer
  solved <- solvedQuestionCountJson ^? key "count" . _Integer
  return QuestionCount {..}

extractQuestionCountList :: Value -> Maybe [QuestionCount]
extractQuestionCountList value = do
  allQuestionsCountJson <- value ^? key "allQuestionsCount" . _Array
  solvedQuestionsCountJson <- value ^? key "matchedUser" . key "submitStatsGlobal" . key "acSubmissionNum" . _Array
  fmap V.toList $ V.mapM extractQuestionCount $ V.zip allQuestionsCountJson solvedQuestionsCountJson

extractUserInfo :: Value -> Value -> Maybe UserInfo
extractUserInfo userInfoJson questionCountJson = do
  username <- fmap T.unpack $ userInfoJson ^? key "user" . key "username" . _String
  isCurrentUserPremium <- userInfoJson ^? key "user" . key "isCurrentUserPremium" . _Bool
  questionCountList <- extractQuestionCountList questionCountJson
  return
    UserInfo
      { username = username,
        premium = isCurrentUserPremium,
        allQuestionCount = head questionCountList,
        easyQuestionCount = questionCountList !! 1,
        mediumQuestionCount = questionCountList !! 2,
        hardQuestionCount = questionCountList !! 3
      }

getUserInfo :: IO UserInfo
getUserInfo = do
  userInfoJson <- requestUserInfo >>= getResponseBody
  case userInfoJson of
    Null -> error "Please login in to leetcode in Chrome and try again"
    _ -> do
      let username = fmap T.unpack $ userInfoJson ^? key "user" . key "username" . _String
      case username of
        Nothing -> error "Please login in to leetcode in Chrome and try again"
        Just username -> do
          questionCountJson <- requestUserQuestionCount username >>= getResponseBody
          case extractUserInfo userInfoJson questionCountJson of
            Just userInfo -> return userInfo
            Nothing -> error "Error parsing UserInfo"

-- return $
--   map
--     ( \(key, maybe) ->
--         case maybe of
--           Just (String x) -> (key, init . tail . show $ x)
--           Just (Bool x) -> (key, show x)
--           _ -> error "Need to re login in the browser"
--     )
--     [("username", username), ("premium", isCurrentUserPremium)]
