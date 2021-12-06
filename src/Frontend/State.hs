module Frontend.State where

import Backend.Problem (Difficulty, Problem (Problem), Status)
import Backend.ProblemDetail (ProblemDetail (ProblemDetail))
import Backend.UserInfo (UserInfo)
import Brick (App (..), BrickEvent (VtyEvent), EventM, Next)
import Brick.Widgets.List as BL
import Data.Vector as V

data Focus = ProblemFocus | DetailFocus deriving (Show, Eq)

data TuiState = TuiState
  { tuiStateUserInfo :: UserInfo,
    tuiStateProblems :: V.Vector Problem,
    tuiStateProblemList :: BL.List ResourceName Problem,
    tuiStateCurrentFocus :: Focus,
    tuiStateProblemDetail :: Maybe ProblemDetailList
  }
  deriving (Show)

data ProblemDetailList = ProblemDetailList
  { slug :: String,
    content :: String,
    codeDefinitionList :: BL.List ResourceName (String, String)
  }
  deriving (Show)

type NewState = EventM ResourceName (Next TuiState)

data ResourceName
  = ProblemView
  | DetailView
  deriving (Eq, Ord, Show)
