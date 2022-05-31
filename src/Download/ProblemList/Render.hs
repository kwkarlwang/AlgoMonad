{-# OPTIONS -Wunused-imports #-}
module Download.ProblemList.Render where

import Brick
  ( Padding (Max, Pad),
    TextWidth (textWidth),
    Widget,
    hBox,
    padLeftRight,
    padRight,
  )
import Brick.Widgets.List as BL hiding (reverse)
import Data.Vector as V hiding ((++))
import Download.ProblemList.State
  ( Difficulty (Easy, Hard, Medium),
    Problem (difficulty, pid, status, title, totalAccept, totalSubmit),
    Status (Cleared, NotAttempted, NotCleared),
  )
import Frontend.State (ResourceName)
import Frontend.Utils (drawGreen, drawRed, drawSelectedOrUnfocus, drawStr, drawYellow, floatDiv, floatRound)

renderProblem :: Bool -> BL.List ResourceName Problem -> Widget ResourceName
renderProblem hasFocus problemList = BL.renderList renderFunc hasFocus problemList
  where
    renderFunc isSelected problem = hBox components
      where
        components =
          [ renderStatus hasFocus isSelected problem,
            padRight Max $ renderTitle hasFocus maxTitleWidth isSelected problem,
            renderDifficulty hasFocus maxDifficultyWidth isSelected problem,
            renderPercent hasFocus maxPercentWidth isSelected problem
          ]

    problemVector = BL.listElements problemList
    maxTitleWidth = V.maximum $ V.map (textWidth . showTitle) problemVector
    maxDifficultyWidth = V.maximum $ V.map (textWidth . showDifficulty) problemVector
    maxPercentWidth = V.maximum (V.map (textWidth . showPercent) problemVector)

renderStatus :: Bool -> Bool -> Problem -> Widget ResourceName
renderStatus hasFocus isSelected problem = padLeftRight 1 widget
  where
    currentStatus = status problem
    widget =
      drawSelectedOrUnfocus
        isSelected
        hasFocus
        ( case currentStatus of
            Cleared -> drawGreen
            NotCleared -> drawRed
            NotAttempted -> drawStr
        )
        $ showStatus
          problem

renderTitle :: Bool -> Int -> Bool -> Problem -> Widget ResourceName
renderTitle hasFocus maxPad isSelected problem = padRight (Pad (maxPad - titleWidth)) widget
  where
    titleString = showTitle problem
    widget = drawSelectedOrUnfocus isSelected hasFocus drawStr titleString
    titleWidth = textWidth titleString

renderDifficulty :: Bool -> Int -> Bool -> Problem -> Widget ResourceName
renderDifficulty hasFocus maxPad isSelected problem = padRight (Pad (maxPad - difficultyWidth + 4)) widget
  where
    difficultyString = showDifficulty problem
    widget =
      drawSelectedOrUnfocus
        isSelected
        hasFocus
        ( case difficulty problem of
            Easy -> drawGreen
            Medium -> drawYellow
            Hard -> drawRed
        )
        difficultyString
    difficultyWidth = textWidth difficultyString

renderPercent :: Bool -> Int -> Bool -> Problem -> Widget ResourceName
renderPercent hasFocus maxPad isSelected problem = padRight (Pad (maxPad - percentWidth)) widget
  where
    percentString = showPercent problem
    widget = drawSelectedOrUnfocus isSelected hasFocus drawStr percentString
    percentWidth = textWidth percentString

showStatus :: Problem -> String
showStatus = show . status

showTitle :: Problem -> String
showTitle problem = show (pid problem) ++ " " ++ title problem

showDifficulty :: Problem -> String
showDifficulty = show . difficulty

showPercent :: Problem -> String
showPercent problem = (show . getPercent) problem ++ "% "

getPercent :: Problem -> Float
getPercent problem = floatRound (100 * decimal) 2
  where
    decimal = totalAccept problem `floatDiv` totalSubmit problem
