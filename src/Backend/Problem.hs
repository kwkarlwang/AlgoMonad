{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wunused-imports #-}

module Backend.Problem where

import Backend.Utils
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Ord (comparing)
import Data.Vector as V hiding (init, tail)
import Data.Vector.Algorithms.Intro (sortBy)
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
    status :: Status,
    slug :: String
  }
  deriving (Eq, Show)

requestProblems :: (FromJSON a) => IO (Req (JsonResponse a))
requestProblems = req GET (https "leetcode.com" /: "api" /: "problems" /: "algorithms") NoReqBody jsonResponse <$> getCredentials

getProblem :: Value -> Maybe Problem
getProblem value = do
  pid <- value ^? key "stat" . key "frontend_question_id"
  title <- value ^? key "stat" . key "question__title"
  difficulty <- value ^? key "difficulty" . key "level"
  paidOnly <- value ^? key "paid_only"
  totalAccept <- value ^? key "stat" . key "total_acs"
  totalSubmit <- value ^? key "stat" . key "total_submitted"
  let status = value ^? key "status"
  slug <- value ^? key "stat" . key "question__title_slug"
  case (pid, title, difficulty, paidOnly, totalAccept, totalSubmit, status, slug) of
    ( Integer pid,
      String title,
      Integer difficulty,
      Bool paidOnly,
      Integer totalAccept,
      Integer totalSubmit,
      status,
      String slug
      ) ->
        return $ Problem pid (init . tail $ show title) diff paidOnly totalAccept totalSubmit stat (init . tail $ show slug)
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
    _ -> Nothing

getProblems :: IO (V.Vector Problem)
getProblems = do
  request <- requestProblems
  r <- runReq defaultHttpConfig request
  let reqData = responseBody r :: Value
  return $ case reqData of
    Null -> V.empty
    _ ->
      let problems = reqData ^? key "stat_status_pairs"
       in case problems of
            Just (Array array) -> do
              case V.mapM getProblem array of
                Just problems -> V.modify (sortBy (comparing pid)) problems
                Nothing -> V.empty
            _ -> V.empty
