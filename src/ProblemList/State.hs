{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wunused-imports #-}

module ProblemList.State where

data Status = Cleared | NotCleared | NotAttempted deriving (Eq)

instance Show Status where
  show Cleared = " ✔"
  show NotCleared = " ✘"
  show NotAttempted = "  "

data Difficulty = Easy | Medium | Hard deriving (Eq, Show)

data Problem = Problem
  { pid :: Integer,
    submitPid :: Integer,
    title :: String,
    difficulty :: Difficulty,
    paidOnly :: Bool,
    totalAccept :: Integer,
    totalSubmit :: Integer,
    status :: Status,
    slug :: String
  }
  deriving (Eq, Show)
