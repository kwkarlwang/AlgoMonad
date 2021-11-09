{-# LANGUAGE OverloadedStrings #-}

module Request where

import Brick (Widget)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Network.HTTP.Req

csrfToken :: ByteString
csrfToken = "TCUigYIA0zliaFoYqX1OJ5uwiq6ZWJFGItWEa2mFE8aPi87pvvjUpvFnNJbwRznv"

leetcodeSession :: ByteString
leetcodeSession = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJfYXV0aF91c2VyX2lkIjoiMjAzNDM3MCIsIl9hdXRoX3VzZXJfYmFja2VuZCI6ImFsbGF1dGguYWNjb3VudC5hdXRoX2JhY2tlbmRzLkF1dGhlbnRpY2F0aW9uQmFja2VuZCIsIl9hdXRoX3VzZXJfaGFzaCI6IjM3ZDgxYzI0NTFkZWIxOTAzZDk2ZTMzMTY1YzIwMzgwMTc3YTYxMDciLCJpZCI6MjAzNDM3MCwiZW1haWwiOiJrYXdhbmdAdWNzZC5lZHUiLCJ1c2VybmFtZSI6Imthd2FuZyIsInVzZXJfc2x1ZyI6Imthd2FuZyIsImF2YXRhciI6Imh0dHBzOi8vYXNzZXRzLmxlZXRjb2RlLmNvbS91c2Vycy9rYXdhbmcvYXZhdGFyXzE1NjEyMjg5ODMucG5nIiwicmVmcmVzaGVkX2F0IjoxNjM1Mzk4NTE4LCJpcCI6Ijc1LjgwLjEwOC4xNzIiLCJpZGVudGl0eSI6IjE3MmZhMWUzYTU3MDk3MmE5NTIzODgwZjA0MzExMThkIiwic2Vzc2lvbl9pZCI6MTQwNzgyNDV9.PSRHH2cs5NRDCwPx0NMI6N8TCJXSw5NMDiUaeBk0HrI"

getCredentials :: Option scheme
getCredentials = header "Cookie" $ "csrftoken=" <> csrfToken <> ";LEETCODE_SESSION=" <> leetcodeSession <> ";"

makeHeaders :: [(ByteString, ByteString)] -> Option scheme
makeHeaders = foldr (\(x, y) b -> header x y <> b) mempty

getUserInfo :: (FromJSON a) => Req (JsonResponse a)
getUserInfo =
  req POST (https "leetcode.com" /: "graphql") (ReqBodyJson payload) jsonResponse headers
  where
    payload = object ["query" .= intercalate "\n" ["{", "  user {", "    username", "    isCurrentUserPremium", "  }", "}"]]
    -- headersMap = [("Referer", "https://leetcode.com"), ("Origin", "https://leetcode.com")]
    -- headers = makeHeaders headersMap <> getCredentials
    headers = getCredentials

getCategoryProblems :: (FromJSON a) => Req (JsonResponse a)
getCategoryProblems = req GET (https "leetcode.com" /: "api" /: "problems" /: "algorithms") NoReqBody jsonResponse headers
  where
    headers = getCredentials

getResponseBody :: Req (JsonResponse Value) -> IO Value
getResponseBody request = do
  r <- runReq defaultHttpConfig request
  let res = responseBody r :: Value
  maybeToIO $ res ^? key "data"

maybeToIO :: Maybe Value -> IO Value
maybeToIO Nothing = return $ Bool False
maybeToIO (Just x) = return x
