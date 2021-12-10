{-# LANGUAGE OverloadedStrings #-}

module TestProblem where

import Backend.Problem
import Data.Aeson
import Data.Aeson.Lens
import Frontend.Problem
import Test.HUnit

test1 = TestCase (assertEqual "Extract Problem from JSON" (getProblem value) expecetedProblem)
  where
    expecetedProblem = Just $ Problem 1 "test title" Hard True 123 456 Cleared "test-title"
    value =
      object
        [ "stat"
            .= object
              [ "frontend_question_id" .= (1 :: Integer),
                "question__title" .= ("test title" :: String),
                "question__title_slug" .= ("test-title" :: String),
                "total_acs" .= (123 :: Integer),
                "total_submitted" .= (456 :: Integer)
              ],
          "difficulty" .= object ["level" .= (3 :: Integer)],
          "status" .= ("ac" :: String),
          "paid_only" .= True
        ]

test2 = TestCase (assertEqual "Extract Problem from corrupt JSON" (getProblem value) expecetedProblem)
  where
    expecetedProblem = Nothing
    value =
      object
        [ "sta"
            .= object
              [ "frontend_question_id" .= (1 :: Integer),
                "question__title" .= ("test title" :: String),
                "question__title_slug" .= ("test-title" :: String),
                "total_acs" .= (123 :: Integer),
                "total_submitted" .= (456 :: Integer)
              ],
          "difficulty" .= object ["level" .= (3 :: Integer)],
          "status" .= ("ac" :: String),
          "paid_only" .= True
        ]

test3 = TestCase (assertEqual "Extract Problem from JSON" (getProblem value) expecetedProblem)
  where
    expecetedProblem = Just $ Problem 1 "test title" Easy True 123 456 Cleared "test-title"
    value =
      object
        [ "stat"
            .= object
              [ "frontend_question_id" .= (1 :: Integer),
                "question__title" .= ("test title" :: String),
                "question__title_slug" .= ("test-title" :: String),
                "total_acs" .= (123 :: Integer),
                "total_submitted" .= (456 :: Integer)
              ],
          "difficulty" .= object ["level" .= (1 :: Integer)],
          "status" .= ("ac" :: String),
          "paid_only" .= True
        ]

test4 = TestCase (assertEqual "Extract Problem from JSON" (getProblem value) expecetedProblem)
  where
    expecetedProblem = Just $ Problem 1 "test title" Easy True 123 456 NotAttempted "test-title"
    value =
      object
        [ "stat"
            .= object
              [ "frontend_question_id" .= (1 :: Integer),
                "question__title" .= ("test title" :: String),
                "question__title_slug" .= ("test-title" :: String),
                "total_acs" .= (123 :: Integer),
                "total_submitted" .= (456 :: Integer)
              ],
          "difficulty" .= object ["level" .= (1 :: Integer)],
          "paid_only" .= True
        ]

testShowStatus1 = TestCase (assertEqual "Test showing the status" (showStatus problem) "  ")
  where
    problem = Problem 1 "test title" Easy True 123 456 NotAttempted "test-title"

testShowStatus2 = TestCase (assertEqual "Test showing the status" (showStatus problem) " ✔")
  where
    problem = Problem 1 "test title" Easy True 123 456 Cleared "test-title"

testShowStatus3 = TestCase (assertEqual "Test showing the status" (showStatus problem) " ✘")
  where
    problem = Problem 1 "test title" Easy True 123 456 NotCleared "test-title"

testShowTitle = TestCase (assertEqual "Test showing the title" (showTitle problem) "1 test title")
  where
    problem = Problem 1 "test title" Easy True 123 456 NotCleared "test-title"

testShowDifficulty1 = TestCase (assertEqual "Test showing the difficulty" (showDifficulty problem) "Easy")
  where
    problem = Problem 1 "test title" Easy True 123 456 NotCleared "test-title"

testShowDifficulty2 = TestCase (assertEqual "Test showing the difficulty" (showDifficulty problem) "Medium")
  where
    problem = Problem 1 "test title" Medium True 123 456 NotCleared "test-title"

testShowDifficulty3 = TestCase (assertEqual "Test showing the difficulty" (showDifficulty problem) "Hard")
  where
    problem = Problem 1 "test title" Hard True 123 456 NotCleared "test-title"

testShowPercent = TestCase (assertEqual "Test showing the percent" (showPercent problem) "26.97% ")
  where
    problem = Problem 1 "test title" Hard True 123 456 NotCleared "test-title"

tests =
  [ test1,
    test2,
    test3,
    test4,
    testShowStatus1,
    testShowStatus2,
    testShowStatus3,
    testShowTitle,
    testShowDifficulty1,
    testShowDifficulty2,
    testShowDifficulty3
  ]
