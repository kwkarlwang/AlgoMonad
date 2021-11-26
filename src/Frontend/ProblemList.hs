module Frontend.ProblemList where

import Backend.Problem (Problem (Problem, difficulty, pid, status, title, totalAccept, totalSubmit), Status (Cleared, NotAttempted, NotCleared))
import Brick (ViewportType (Vertical), Widget, continue, hBox, padLeftRight, str, vBox, viewport, visible)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List as BL hiding (reverse)
import Data.List (intercalate)
import Data.Vector as V hiding (map, (++))
import Frontend.State (NewState, ResourceName (Viewport1), TuiState (TuiState, tuiStateProblems))
import Frontend.Utils (drawStr, floatDiv, floatRound)

type SelectF = BL.List String Problem -> Maybe (BL.List String Problem)

-- select :: SelectF -> TuiState -> NewState
-- select selectF s = case selectF . tuiStateProblems $ s of
--   Nothing -> continue s
--   Just problems -> continue $ s {tuiStateProblems = problems}

-- renderProblem ::

renderProblem :: BL.GenericList ResourceName V.Vector Problem -> Widget ResourceName
renderProblem problems = components
  where
    components = BL.renderList renderTitle True problems

-- components = [renderStatus, renderTitle, renderDifficulty, renderPercent]

-- renderStatus :: Problem -> Widget ResourceName
-- renderStatus =
--   renderColumn
--     " "
--     ( \problem -> case status problem of
--         Cleared -> "✔"
--         NotCleared -> "✘"
--         NotAttempted -> " "
--     )

-- renderId :: BL.List Problem -> Widget ResourceName
-- renderId = renderColumn "ID" $ show . pid

renderTitle :: Bool -> Problem -> Widget ResourceName
renderTitle bool = drawStr bool . (\problem -> show (pid problem) ++ " " ++ title problem)

-- renderDifficulty :: Problem -> Widget ResourceName
-- renderDifficulty = renderColumn "Difficulty" $ show . difficulty

-- renderCount :: BL.List Problem -> Widget ResourceName
-- renderCount = renderColumn "Accept/Submit" (\problem -> show (totalAccept problem) ++ "/" ++ show (totalSubmit problem))

-- renderPercent :: Problem -> Widget ResourceName
-- renderPercent =
--   renderColumn
--     "Percent"
--     ( \problem ->
--         let decimal = totalAccept problem `floatDiv` totalSubmit problem
--          in show $ floatRound (100 * decimal) 2
--     )

-- renderColumn :: String -> (Problem -> String) -> Problem -> Widget ResourceName
-- renderColumn headerTitle renderFunc problems = str $ renderFunc problems

-- vBox $
--   concat
--     [ [str headerTitle],
--       map (drawStr False . renderFunc) $ reverse $ nonEmptyCursorPrev problems,
--       [visible $ drawStr True $ renderFunc $ nonEmptyCursorCurrent problems],
--       map (drawStr False . renderFunc) $ nonEmptyCursorNext problems
--     ]
