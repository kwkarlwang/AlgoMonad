{-# OPTIONS -Wunused-imports #-}
module Frontend.ProblemDetail where

import Brick (Padding (Max), Widget, hBox, padBottom, (<=>))
import Brick.Widgets.List as BL hiding (reverse)
import Frontend.State (ProblemDetailList (codeSnippets, dislikes, likes), ResourceName)
import Frontend.Utils (drawGreen, drawRed, drawSelected, drawStr)

renderProblemDetail :: Bool -> ProblemDetailList -> Widget ResourceName
renderProblemDetail hasFocus problemDetail = BL.renderList renderFunc hasFocus codeList <=> renderLikesAndDislikes (likes problemDetail) (dislikes problemDetail)
  where
    codeList = codeSnippets problemDetail
    codeVector = BL.listElements codeList
    renderFunc = renderLanguage

renderLanguage :: Bool -> (String, String) -> Widget ResourceName
renderLanguage isSelected = drawSelected isSelected drawStr . fst

renderLikesAndDislikes :: Integer -> Integer -> Widget ResourceName
renderLikesAndDislikes likes dislikes = widget
  where
    likesWidget = hBox [drawStr "👍: ", drawGreen $ show likes]
    dislikesWidget = hBox [drawStr "   👎: ", drawRed $ show dislikes]
    widget = padBottom Max $ drawStr " " <=> hBox [likesWidget, dislikesWidget]
