{-# LANGUAGE OverloadedStrings #-}

module TestProblemDetail where

import Backend.ProblemDetail
import Data.Aeson
import Data.Aeson.Lens
import Data.Vector
import Test.HUnit

test1 = TestCase $ assertEqual "Extract ProblemDetail from JSON" (extractProblemDetail "test-title" value) expecetedProblemDetail
  where
    expecetedProblemDetail = Just $ ProblemDetail "test-title" "test content" $ fromList [("c", "C code"), ("python3", "Python code")]
    value =
      object
        [ "question"
            .= object
              [ "content" .= ("test content" :: String),
                "codeDefinition" .= ("[{\"value\": \"c\", \"text\": \"C\", \"defaultCode\": \"C code\"}, {\"value\": \"python3\", \"text\": \"Python 3\", \"defaultCode\": \"Python code\"}]" :: String)
              ]
        ]

test2 = TestCase $ assertEqual "Extract ProblemDetail from corrupt JSON" (extractProblemDetail "test-title" value) expecetedProblemDetail
  where
    expecetedProblemDetail = Nothing
    value =
      object
        [ "question"
            .= object
              [ "contet" .= ("test content" :: String),
                "codeDefinition" .= ("[{\"value\": \"c\", \"text\": \"C\", \"defaultCode\": \"C code\"}, {\"value\": \"python3\", \"text\": \"Python 3\", \"defaultCode\": \"Python code\"}]" :: String)
              ]
        ]

test3 = TestCase $ assertEqual "Download question" (extractProblemDetail "test-title" value) expecetedProblemDetail
  where
    expecetedProblemDetail = Nothing
    value =
      object
        [ "question"
            .= object
              [ "contet" .= ("test content" :: String),
                "codeDefinition" .= ("[{\"value\": \"c\", \"text\": \"C\", \"defaultCode\": \"C code\"}, {\"value\": \"python3\", \"text\": \"Python 3\", \"defaultCode\": \"Python code\"}]" :: String)
              ]
        ]

tests = [test1, test2]
