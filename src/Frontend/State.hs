module Frontend.State where

import Backend.Problem (Difficulty, Problem (Problem), Status)
import Backend.ProblemDetail (ProblemDetail (ProblemDetail))
import Backend.Submission (Submission)
import Backend.UserInfo (UserInfo)
import Brick (App (..), BrickEvent (VtyEvent), EventM, Next)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as BL
import qualified Data.Vector as V

data Focus
  = ListFocus
  | DetailFocus
  | SearchFocus
  deriving (Show, Eq)

data Tab = DownloadTab | SubmissionTab deriving (Show, Eq)

data TuiState = TuiState
  { tuiStateUserInfo :: UserInfo,
    -- Download
    tuiStateProblemList :: BL.List ResourceName Problem,
    tuiStateDownloadFocus :: Focus,
    tuiStateProblemDetail :: Maybe ProblemDetailList,
    tuiStateDownloadSearch :: E.Editor String ResourceName,
    tuiStateMessage :: Maybe String,
    -- Submission
    tuiStateSubmissionFocus :: Focus,
    tuiStateSubmissionSearch :: E.Editor String ResourceName,
    tuiStateSubmissionDetail :: Maybe (BL.List ResourceName FilePath),
    tuiStateSubmissionList :: BL.List ResourceName Submission,
    tuiStateTab :: Tab
  }
  deriving (Show)

data ProblemDetailList = ProblemDetailList
  { pid :: Integer,
    slug :: String,
    content :: String,
    codeDefinitionList :: BL.List ResourceName (String, String)
  }
  deriving (Show)

type NewState = EventM ResourceName (Next TuiState)

data ResourceName
  = DownloadListView
  | DownloadDetailView
  | DownloadSearchView
  | SubmissionListView
  | SubmissionDetailView
  | SubmissionSearchView
  deriving (Eq, Ord, Show)
