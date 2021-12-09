module Frontend.State where

import Backend.Problem (Difficulty, Problem (Problem), Status)
import Backend.ProblemDetail (ProblemDetail (ProblemDetail))
import Backend.UserInfo (UserInfo)
import Brick (App (..), BrickEvent (VtyEvent), EventM, Next)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as BL
import qualified Data.Vector as V

data Focus = ProblemFocus | DetailFocus | SearchFocus deriving (Show, Eq)

data TuiState = TuiState
  { tuiStateUserInfo :: UserInfo,
    tuiStateProblemList :: BL.List ResourceName Problem,
    tuiStateCurrentFocus :: Focus,
    tuiStateProblemDetail :: Maybe ProblemDetailList,
    tuiStateSearch :: E.Editor String ResourceName,
    tuiStateMessage :: Maybe String
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
  | SearchView
  deriving (Eq, Ord, Show)
