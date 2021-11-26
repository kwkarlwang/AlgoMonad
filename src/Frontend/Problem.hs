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

renderProblem :: NonEmptyCursor Problem -> Widget ResourceName
renderProblem problems = viewport Viewport1 Vertical . hBox $ map (\f -> padLeftRight 1 $ f problems) components
  where
    components = [renderStatus, renderTitle, renderDifficulty, renderPercent]

renderStatus :: NonEmptyCursor Problem -> Widget ResourceName
renderStatus =
  renderColumn
    " "
    ( \problem -> case status problem of
        Cleared -> "✔"
        NotCleared -> "✘"
        NotAttempted -> " "
    )

-- renderId :: NonEmptyCursor Problem -> Widget ResourceName
-- renderId = renderColumn "ID" $ show . pid

renderTitle :: NonEmptyCursor Problem -> Widget ResourceName
renderTitle = renderColumn "Title" (\problem -> show (pid problem) ++ " " ++ title problem)

renderDifficulty :: NonEmptyCursor Problem -> Widget ResourceName
renderDifficulty = renderColumn "Difficulty" $ show . difficulty

-- renderCount :: NonEmptyCursor Problem -> Widget ResourceName
-- renderCount = renderColumn "Accept/Submit" (\problem -> show (totalAccept problem) ++ "/" ++ show (totalSubmit problem))

renderPercent :: NonEmptyCursor Problem -> Widget ResourceName
renderPercent =
  renderColumn
    "Percent"
    ( \problem ->
        let decimal = totalAccept problem `floatDiv` totalSubmit problem
         in show $ floatRound (100 * decimal) 2
    )

renderColumn :: String -> (Problem -> String) -> NonEmptyCursor Problem -> Widget ResourceName
renderColumn headerTitle renderFunc problems =
  vBox $
    concat
      [ [str headerTitle],
        map (drawStr False . renderFunc) $ reverse $ nonEmptyCursorPrev problems,
        [visible $ drawStr True $ renderFunc $ nonEmptyCursorCurrent problems],
        map (drawStr False . renderFunc) $ nonEmptyCursorNext problems
      ]
