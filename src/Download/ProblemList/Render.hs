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
    Problem (difficulty, paidOnly, pid, status, title, totalAccept, totalSubmit),
    Status (Cleared, NotAttempted, NotCleared),
  )
import Frontend.State (ResourceName)
import Frontend.Utils (drawGreen, drawRed, drawSelectedOrUnfocus, drawStr, drawYellow, floatDiv, floatRound)

renderProblem :: Bool -> BL.List ResourceName Problem -> Widget ResourceName
renderProblem hasFocus problemList = BL.renderList renderFunc hasFocus problemList
  where
    renderFunc :: Bool -> Problem -> Widget ResourceName
    renderFunc isSelected problem = hBox components
      where
        components =
          Prelude.map
            (\f -> f hasFocus isSelected problem)
            [ renderStatus,
              \f s p -> padRight Max $ renderTitle maxTitleWidth f s p,
              renderDifficulty maxDifficultyWidth,
              renderPaidOnly,
              renderPercent maxPercentWidth
            ]

    problemVector = BL.listElements problemList
    maxTitleWidth = V.maximum $ V.map (textWidth . showTitle) problemVector
    maxDifficultyWidth = V.maximum $ V.map (textWidth . showDifficulty) problemVector
    maxPercentWidth = V.maximum (V.map (textWidth . showPercent) problemVector)

renderStatus :: Bool -> Bool -> Problem -> Widget ResourceName
renderStatus hasFocus isSelected problem = widget
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
        $ showStatus problem ++ " "

renderTitle :: Int -> Bool -> Bool -> Problem -> Widget ResourceName
renderTitle maxPad hasFocus isSelected problem = padRight (Pad (maxPad - titleWidth)) widget
  where
    titleString = showTitle problem
    widget = drawSelectedOrUnfocus isSelected hasFocus drawStr titleString
    titleWidth = textWidth titleString

renderDifficulty :: Int -> Bool -> Bool -> Problem -> Widget ResourceName
renderDifficulty maxPad hasFocus isSelected problem = padRight (Pad (maxPad - difficultyWidth)) widget
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

renderPaidOnly :: Bool -> Bool -> Problem -> Widget ResourceName
renderPaidOnly hasFocus isSelected problem = widget
  where
    paidSymbol = if paidOnly problem then "$" else " "
    widget = padLeftRight 2 $ drawSelectedOrUnfocus isSelected hasFocus drawGreen paidSymbol

renderPercent :: Int -> Bool -> Bool -> Problem -> Widget ResourceName
renderPercent maxPad hasFocus isSelected problem = padRight (Pad (maxPad - percentWidth)) widget
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
