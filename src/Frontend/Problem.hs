module Frontend.Problem where

import Backend.Problem (Problem (Problem, difficulty, paidOnly, pid, status, title, totalAccept, totalSubmit), Status (Cleared, NotAttempted, NotCleared))
import Brick (Padding (Pad), TextWidth (textWidth), ViewportType (Vertical), Widget, continue, hBox, padLeftRight, padRight, str, vBox, viewport, visible)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List as BL hiding (reverse)
import Data.List (intercalate)
import Data.Vector as V hiding ((++))
import Frontend.State (NewState, ResourceName (ProblemView), TuiState (TuiState, tuiStateProblems))
import Frontend.Utils (drawStr, floatDiv, floatRound)

renderProblem :: Bool -> BL.List ResourceName Problem -> Widget ResourceName
renderProblem hasFocus problemList = BL.renderList renderFunc hasFocus problemList
  where
    renderFunc bool problem = hBox $ Prelude.map (\f -> f bool problem) components
    -- components =
    --   if hasFocus
    --     then
    --       [ renderStatus,
    --         renderTitle maxTitleWidth,
    --         renderDifficulty maxDifficultyWidth,
    --         renderPercent maxPercentWidth
    --       ]
    --     else
    --       [ renderStatus,
    --         renderTitle maxTitleWidth
    --       ]
    components =
      [ renderStatus,
        renderTitle maxTitleWidth,
        renderDifficulty maxDifficultyWidth,
        renderPercent maxPercentWidth
      ]

    problemVector = BL.listElements problemList
    maxTitleWidth = V.maximum $ V.map (textWidth . showTitle) problemVector
    maxDifficultyWidth = V.maximum $ V.map (textWidth . showDifficulty) problemVector
    maxPercentWidth = V.maximum $ V.map (textWidth . showPercent) problemVector

renderStatus :: Bool -> Problem -> Widget ResourceName
renderStatus bool problem = padLeftRight 1 $ drawStr bool . showStatus $ problem

renderTitle :: Int -> Bool -> Problem -> Widget ResourceName
renderTitle maxPad bool problem = padRight (Pad (maxPad - titleWidth)) widget
  where
    titleString = showTitle problem
    widget = drawStr bool titleString
    titleWidth = textWidth titleString

renderDifficulty :: Int -> Bool -> Problem -> Widget ResourceName
renderDifficulty maxPad bool problem = padRight (Pad (maxPad - difficultyWidth + 2)) widget
  where
    difficultyString = showDifficulty problem
    widget = drawStr bool difficultyString
    difficultyWidth = textWidth difficultyString

renderPercent :: Int -> Bool -> Problem -> Widget ResourceName
renderPercent maxPad bool problem = padRight (Pad (maxPad - percentWidth)) widget
  where
    percentString = showPercent problem
    widget = drawStr bool percentString
    percentWidth = textWidth percentString

showStatus :: Problem -> String
showStatus = show . status

showTitle :: Problem -> String
showTitle problem = show (pid problem) ++ " " ++ title problem

showDifficulty :: Problem -> String
showDifficulty = show . difficulty

showPercent :: Problem -> String
showPercent problem = (show . getPercent) problem ++ "%"

getPercent :: Problem -> Float
getPercent problem = floatRound (100 * decimal) 2
  where
    decimal = totalAccept problem `floatDiv` totalSubmit problem
