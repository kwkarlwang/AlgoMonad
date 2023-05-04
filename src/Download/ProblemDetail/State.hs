{-# OPTIONS -Wunused-imports #-}

module Download.ProblemDetail.State where

import qualified Data.Vector as V

data ProblemDetail = ProblemDetail
  { pid :: Integer,
    slug :: String,
    content :: String,
    codeDefinitionVector :: V.Vector (String, String)
  }
  deriving (Eq, Show)
