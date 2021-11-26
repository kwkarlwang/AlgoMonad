module Frontend.Problem where

import Backend.Problem (Problem (Problem, difficulty, pid, status, title, totalAccept, totalSubmit), Status (Cleared, NotAttempted, NotCleared))
import Brick (ViewportType (Vertical), Widget, continue, hBox, padLeftRight, str, vBox, viewport, visible)
import Brick.Widgets.Center (hCenter)
import Cursor.Simple.List.NonEmpty (NonEmptyCursor, nonEmptyCursorCurrent, nonEmptyCursorNext, nonEmptyCursorPrev, nonEmptyCursorSelectNext)
import Data.List (intercalate)
import Frontend.State (NewState, ResourceName (Viewport1), TuiState (TuiState, tuiRenderProblems))
import Frontend.Utils (drawStr, floatDiv, floatRound)

type SelectF = NonEmptyCursor Problem -> Maybe (NonEmptyCursor Problem)

select :: SelectF -> TuiState -> NewState
select selectF s = case selectF . tuiRenderProblems $ s of
  Nothing -> continue s
  Just problems -> continue $ s {tuiRenderProblems = problems}

showProblem :: NonEmptyCursor Problem -> Widget ResourceName
showProblem problems = viewport Viewport1 Vertical . hBox $ map (\f -> padLeftRight 1 $ f problems) components
  where
    components = [showStatus, showTitle, showDifficulty, showPercent]

showStatus :: NonEmptyCursor Problem -> Widget ResourceName
showStatus =
  showColumn
    " "
    ( \problem -> case status problem of
        Cleared -> "✔"
        NotCleared -> "✘"
        NotAttempted -> " "
    )

-- showId :: NonEmptyCursor Problem -> Widget ResourceName
-- showId = showColumn "ID" $ show . pid

showTitle :: NonEmptyCursor Problem -> Widget ResourceName
showTitle = showColumn "Title" (\problem -> show (pid problem) ++ " " ++ title problem)

showDifficulty :: NonEmptyCursor Problem -> Widget ResourceName
showDifficulty = showColumn "Difficulty" $ show . difficulty

-- showCount :: NonEmptyCursor Problem -> Widget ResourceName
-- showCount = showColumn "Accept/Submit" (\problem -> show (totalAccept problem) ++ "/" ++ show (totalSubmit problem))

showPercent :: NonEmptyCursor Problem -> Widget ResourceName
showPercent =
  showColumn
    "Percent"
    ( \problem ->
        let decimal = totalAccept problem `floatDiv` totalSubmit problem
         in show $ floatRound (100 * decimal) 2
    )

showColumn :: String -> (Problem -> String) -> NonEmptyCursor Problem -> Widget ResourceName
showColumn headerTitle renderFunc problems =
  vBox $
    concat
      [ [str headerTitle],
        map (drawStr False . renderFunc) $ reverse $ nonEmptyCursorPrev problems,
        [visible $ drawStr True $ renderFunc $ nonEmptyCursorCurrent problems],
        map (drawStr False . renderFunc) $ nonEmptyCursorNext problems
      ]
