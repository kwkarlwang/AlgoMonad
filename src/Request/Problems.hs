{-# LANGUAGE OverloadedStrings #-}

module Request.Problems where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.List (sortOn)
import Data.Vector (toList)
import Network.HTTP.Req
import Request.Utils

data Problem = Problem
  { pid :: Integer,
    title :: String,
    difficulty :: Integer,
    paidOnly :: Bool,
    totalAccept :: Integer,
    totalSubmit :: Integer
  }
  deriving (Eq, Show)

type Problems = [Problem]

requestProblems :: (FromJSON a) => Req (JsonResponse a)
requestProblems = req GET (https "leetcode.com" /: "api" /: "problems" /: "algorithms") NoReqBody jsonResponse headers
  where
    headers = getCredentials

getProblem :: Value -> Problem
getProblem value = problem
  where
    pid = value ^? key "stat" . key "frontend_question_id"
    title = value ^? key "stat" . key "question__title"
    difficulty = value ^? key "difficulty" . key "level"
    paidOnly = value ^? key "paid_only"
    totalAccept = value ^? key "stat" . key "total_acs"
    totalSubmit = value ^? key "stat" . key "total_submitted"
    problem = case (pid, title, difficulty, paidOnly, totalAccept, totalSubmit) of
      (Just (Integer pid), Just (String title), Just (Integer difficulty), Just (Bool paidOnly), Just (Integer totalAccept), Just (Integer totalSubmit)) ->
        Problem pid (init . tail $ show title) difficulty paidOnly totalAccept totalSubmit
      _ -> error "failed to key"

getProblems :: IO Problems
getProblems = do
  r <- runReq defaultHttpConfig requestProblems
  let reqData = responseBody r :: Value
  return $ case reqData of
    Bool False -> []
    _ ->
      let problems = reqData ^? key "stat_status_pairs"
       in case problems of
            -- Just (Array array) -> sortOn pid $ map getProblem (toList array)
            Just (Array array) -> reverse $ map getProblem (toList array)
            _ -> []
