module Frontend.State where

import Backend.Problem (Problem (Problem))
import Backend.UserInfo (UserInfo)
import Brick (App (..), BrickEvent (VtyEvent), EventM, Next)
import Brick.Widgets.List as BL
import Cursor.Simple.List.NonEmpty (NonEmptyCursor)
import Data.Vector as V

data TuiState = TuiState {tuiStateUserInfo :: UserInfo, tuiStateProblems :: BL.GenericList ResourceName V.Vector Problem, tuiRenderProblems :: NonEmptyCursor Problem}
  deriving (Show)

type NewState = EventM ResourceName (Next TuiState)

data ResourceName = Viewport1 deriving (Eq, Ord, Show)
