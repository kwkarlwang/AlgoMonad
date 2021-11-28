{-# LANGUAGE OverloadedStrings #-}

module Backend.Problem where

import Backend.Utils
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Vector as V hiding (init, tail)
import Network.HTTP.Req

data Status = Cleared | NotCleared | NotAttempted deriving (Eq)

instance Show Status where
  show Cleared = " ✔"
  show NotCleared = " ✘"
  show NotAttempted = "  "

data Difficulty = Easy | Medium | Hard deriving (Eq, Show)

data Problem = Problem
  { pid :: Integer,
    title :: String,
    difficulty :: Difficulty,
    paidOnly :: Bool,
    totalAccept :: Integer,
    totalSubmit :: Integer,
    status :: Status
  }
  deriving (Eq, Show)

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
    status = value ^? key "status"
    problem = case (pid, title, difficulty, paidOnly, totalAccept, totalSubmit, status) of
      (Just (Integer pid), Just (String title), Just (Integer difficulty), Just (Bool paidOnly), Just (Integer totalAccept), Just (Integer totalSubmit), status) ->
        Problem pid (init . tail $ show title) diff paidOnly totalAccept totalSubmit stat
        where
          diff = case difficulty of
            1 -> Easy
            2 -> Medium
            3 -> Hard
            _ -> error "unexpected case"
          stat = case status of
            Just (String "notac") -> NotCleared
            Just (String "ac") -> Cleared
            _ -> NotAttempted
      _ -> error "failed to key"

getProblems :: IO (V.Vector Problem)
getProblems = do
  r <- runReq defaultHttpConfig requestProblems
  let reqData = responseBody r :: Value
  return $ case reqData of
    Null -> V.empty
    _ ->
      let problems = reqData ^? key "stat_status_pairs"
       in case problems of
            -- Just (Array array) -> sortOn pid $ map getProblem (toList array)
            Just (Array array) -> V.reverse $ V.map getProblem array
            _ -> V.empty
