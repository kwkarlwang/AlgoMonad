module UserInfo.State where

data UserInfo = UserInfo
  { username :: String,
    premium :: Bool,
    allQuestionCount :: QuestionCount,
    easyQuestionCount :: QuestionCount,
    mediumQuestionCount :: QuestionCount,
    hardQuestionCount :: QuestionCount
  }
  deriving (Show)

data QuestionCount = QuestionCount
  { difficulty :: String,
    count :: Integer,
    solved :: Integer
  }
  deriving (Show)
