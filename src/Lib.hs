{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.String (IsString (fromString))
import Debug.Trace (traceM)
import Network.HTTP.Req

csrfToken :: ByteString
csrfToken = "TCUigYIA0zliaFoYqX1OJ5uwiq6ZWJFGItWEa2mFE8aPi87pvvjUpvFnNJbwRznv"

leetcodeSession :: ByteString
leetcodeSession = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJfYXV0aF91c2VyX2lkIjoiMjAzNDM3MCIsIl9hdXRoX3VzZXJfYmFja2VuZCI6ImFsbGF1dGguYWNjb3VudC5hdXRoX2JhY2tlbmRzLkF1dGhlbnRpY2F0aW9uQmFja2VuZCIsIl9hdXRoX3VzZXJfaGFzaCI6IjM3ZDgxYzI0NTFkZWIxOTAzZDk2ZTMzMTY1YzIwMzgwMTc3YTYxMDciLCJpZCI6MjAzNDM3MCwiZW1haWwiOiJrYXdhbmdAdWNzZC5lZHUiLCJ1c2VybmFtZSI6Imthd2FuZyIsInVzZXJfc2x1ZyI6Imthd2FuZyIsImF2YXRhciI6Imh0dHBzOi8vYXNzZXRzLmxlZXRjb2RlLmNvbS91c2Vycy9rYXdhbmcvYXZhdGFyXzE1NjEyMjg5ODMucG5nIiwicmVmcmVzaGVkX2F0IjoxNjM1Mzk4NTE4LCJpcCI6Ijc1LjgwLjEwOC4xNzIiLCJpZGVudGl0eSI6IjE3MmZhMWUzYTU3MDk3MmE5NTIzODgwZjA0MzExMThkIiwic2Vzc2lvbl9pZCI6MTQwNzgyNDV9.PSRHH2cs5NRDCwPx0NMI6N8TCJXSw5NMDiUaeBk0HrI"

getCredentials :: Option scheme
getCredentials = header "Cookie" $ "csrftoken=" <> csrfToken <> ";LEETCODE_SESSION=" <> leetcodeSession <> ";"

makeHeaders :: [(ByteString, ByteString)] -> Option scheme
makeHeaders = foldr (\(x, y) b -> header x y <> b) mempty

getUserInfo :: (MonadHttp m, FromJSON a) => m (JsonResponse a)
getUserInfo =
  req POST (https "leetcode.com" /: "graphql") (ReqBodyJson payload) jsonResponse headers
  where
    payload = object ["query" .= intercalate "\n" ["{", "  user {", "    username", "    isCurrentUserPremium", "  }", "}"]]
    headersMap = [("Referer", "https://leetcode.com"), ("Origin", "https://leetcode.com")]
    headers = makeHeaders headersMap <> getCredentials

getCategoryProblems :: (MonadHttp m, FromJSON a) => m (JsonResponse a)
getCategoryProblems = req GET (https "leetcode.com" /: "api" /: "problems" /: "algorithms") NoReqBody jsonResponse headers
  where
    -- headersMap =
    --   [ ("X-CSRFToken", csrfToken),
    --     ("X-Requested-With", "XMLHttpRequest")
    --   ]
    -- headers = makeHeaders headersMap <> getCredentials
    headers = getCredentials

someFunc :: IO ()
someFunc = runReq defaultHttpConfig $ do
  r <- getCategoryProblems
  liftIO $ print (responseBody r :: Value)
