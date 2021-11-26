module Frontend.State where

import Backend.Problem (Problem (Problem))
import Backend.UserInfo (UserInfo)
import Brick (App (..), BrickEvent (VtyEvent), EventM, Next)
import Cursor.Simple.List.NonEmpty (NonEmptyCursor)

data TuiState = TuiState {tuiStateUserInfo :: UserInfo, tuiStateProblems :: [Problem], tuiRenderProblems :: NonEmptyCursor Problem}
  deriving (Show, Eq)

type NewState = EventM ResourceName (Next TuiState)

data ResourceName = Viewport1 deriving (Eq, Ord, Show)
