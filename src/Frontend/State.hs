module Frontend.State where

import Backend.Problem (Difficulty, Problem (Problem), Status)
import Backend.UserInfo (UserInfo)
import Brick (App (..), BrickEvent (VtyEvent), EventM, Next)
import Brick.Widgets.List as BL
import Data.Vector as V

data TuiState = TuiState
  { tuiStateUserInfo :: UserInfo,
    tuiStateProblems :: V.Vector Problem,
    tuiStateProblemList :: BL.List ResourceName Problem
  }
  deriving (Show)

type NewState = EventM ResourceName (Next TuiState)

data ResourceName
  = ProblemView
  deriving (Eq, Ord, Show)
