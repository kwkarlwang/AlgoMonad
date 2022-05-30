{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use sortOn" #-}
{-# OPTIONS -Wunused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ProblemList.Request where

import Backend.Utils
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Ord (comparing)
import Data.Text as T
import Data.Vector as V hiding (init, tail)
import Data.Vector.Algorithms.Intro (sortBy)
import Network.HTTP.Req
import ProblemList.State

requestProblems :: (FromJSON a) => IO (Req (JsonResponse a))
requestProblems = req GET (https "leetcode.com" /: "api" /: "problems" /: "algorithms") NoReqBody jsonResponse <$> getCredentials

extractProblem :: Value -> Maybe Problem
extractProblem value = do
  let difficultyMapper 1 = Easy
      difficultyMapper 2 = Medium
      difficultyMapper 3 = Hard
      difficultyMapper _ = error "unexpected case"

  let statusMapper "notac" = NotCleared
      statusMapper "ac" = Cleared
      statusMapper _ = NotAttempted

  pid <- value ^? key "stat" . key "frontend_question_id" . _Integer
  submitPid <- value ^? key "stat" . key "question_id" . _Integer
  title <- T.unpack <$> value ^? key "stat" . key "question__title" . _String
  difficulty <- difficultyMapper <$> value ^? key "difficulty" . key "level" . _Integer
  paidOnly <- value ^? key "paid_only" . _Bool
  totalAccept <- value ^? key "stat" . key "total_acs" . _Integer
  totalSubmit <- value ^? key "stat" . key "total_submitted" . _Integer
  status <- statusMapper . T.unpack <$> (value ^? key "status" . _String)
  slug <- T.unpack <$> value ^? key "stat" . key "question__title_slug" . _String
  return Problem {..}

getProblems :: IO (V.Vector Problem)
getProblems = do
  response <- requestProblems >>= runReq defaultHttpConfig
  let reqData = responseBody response :: Value
  return $ case reqData of
    Null -> V.empty
    _ ->
      let problems = reqData ^? key "stat_status_pairs" . _Array
       in case problems of
            Just array -> do
              case V.mapM extractProblem array of
                Just problems -> V.modify (sortBy (comparing pid)) problems
                Nothing -> V.empty
            _ -> V.empty
