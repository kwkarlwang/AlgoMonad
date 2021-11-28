{-# LANGUAGE OverloadedStrings #-}

module Backend.Utils where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Network.HTTP.Req

makeHeaders :: [(ByteString, ByteString)] -> Option scheme
makeHeaders = foldr (\(x, y) b -> header x y <> b) mempty

csrfToken :: ByteString
csrfToken = "75hAUPxpjTPXnQYVd5z7cSum0mqUSGtfR1w7PH0BT4wP1G4pGJsOxi3nOxod5EQz"

leetcodeSession :: ByteString
leetcodeSession = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJfYXV0aF91c2VyX2lkIjoiMjAzNDM3MCIsIl9hdXRoX3VzZXJfYmFja2VuZCI6ImFsbGF1dGguYWNjb3VudC5hdXRoX2JhY2tlbmRzLkF1dGhlbnRpY2F0aW9uQmFja2VuZCIsIl9hdXRoX3VzZXJfaGFzaCI6IjM3ZDgxYzI0NTFkZWIxOTAzZDk2ZTMzMTY1YzIwMzgwMTc3YTYxMDciLCJpZCI6MjAzNDM3MCwiZW1haWwiOiJrYXdhbmdAdWNzZC5lZHUiLCJ1c2VybmFtZSI6Imthd2FuZyIsInVzZXJfc2x1ZyI6Imthd2FuZyIsImF2YXRhciI6Imh0dHBzOi8vYXNzZXRzLmxlZXRjb2RlLmNvbS91c2Vycy9rYXdhbmcvYXZhdGFyXzE1NjEyMjg5ODMucG5nIiwicmVmcmVzaGVkX2F0IjoxNjM3NDU4OTI4LCJpcCI6Ijc1LjgwLjEwOC4xNzIiLCJpZGVudGl0eSI6IjQyZjFhZmM5Nzk4MGVkODAwN2MxNWU0YTM5NTgzMDViIiwic2Vzc2lvbl9pZCI6MTQ4NjM3MTF9.pinX6XasnuheiekAf9KFXPJo41234Dx3xrMFasGnEqs"

getCredentials :: Option scheme
getCredentials = header "Cookie" $ "csrftoken=" <> csrfToken <> ";LEETCODE_SESSION=" <> leetcodeSession <> ";"

getResponseBody :: Req (JsonResponse Value) -> IO Value
getResponseBody request = do
  r <- runReq defaultHttpConfig request
  let res = responseBody r :: Value
  maybeToIO $ res ^? key "data"

maybeToIO :: Maybe Value -> IO Value
maybeToIO Nothing = return Null
maybeToIO (Just x) = return x
