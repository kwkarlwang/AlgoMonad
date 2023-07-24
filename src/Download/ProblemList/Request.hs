{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wunused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use sortOn" #-}

module Download.ProblemList.Request where

import Backend.Utils
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text as T
import Data.Vector as V hiding (init, tail)
import Data.Vector.Algorithms.Intro (sortBy)
import Download.ProblemList.State
import Network.HTTP.Req

requestProblems :: (FromJSON a) => IO (Req (JsonResponse a))
requestProblems = req GET (https "leetcode.com" /: "api" /: "problems" /: "all") NoReqBody jsonResponse <$> getCredentials

extractProblem :: Value -> Maybe Problem
extractProblem value = do
  let difficultyMapper 1 = Easy
      difficultyMapper 2 = Medium
      difficultyMapper 3 = Hard
      difficultyMapper _ = error "unexpected case"

  let statusMapper (Just "notac") = NotCleared
      statusMapper (Just "ac") = Cleared
      statusMapper _ = NotAttempted

  pid <- value ^? key "stat" . key "frontend_question_id" . _Integer
  submitPid <- value ^? key "stat" . key "question_id" . _Integer
  title <- T.unpack <$> value ^? key "stat" . key "question__title" . _String
  difficulty <- difficultyMapper <$> value ^? key "difficulty" . key "level" . _Integer
  paidOnly <- value ^? key "paid_only" . _Bool
  totalAccept <- value ^? key "stat" . key "total_acs" . _Integer
  totalSubmit <- value ^? key "stat" . key "total_submitted" . _Integer
  let status = statusMapper $ T.unpack <$> (value ^? key "status" . _String)
  slug <- T.unpack <$> value ^? key "stat" . key "question__title_slug" . _String
  return Problem {..}

getProblems :: IO (V.Vector Problem)
getProblems = do
  let failedError = error "Failed to get any problems"
  response <- requestProblems >>= runReq defaultHttpConfig
  let reqData = responseBody response :: Value
  return $
    fromMaybe
      failedError
      ( do
          array <- reqData ^? key "stat_status_pairs" . _Array
          problems <- V.mapM extractProblem array
          return $ V.modify (sortBy (comparing pid)) problems
      )
