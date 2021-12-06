module Frontend.ProblemDetail where

import Brick (Padding (Pad), TextWidth (textWidth), ViewportType (Vertical), Widget, continue, hBox, padLeftRight, padRight, str, vBox, viewport, visible)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List as BL hiding (reverse)
import Data.List (intercalate)
import Data.Vector as V hiding ((++))
import Frontend.State (ProblemDetailList (ProblemDetailList, codeDefinitionList), ResourceName)
import Frontend.Utils (drawStr, floatDiv, floatRound)

renderProblemDetail :: Bool -> ProblemDetailList -> Widget ResourceName
renderProblemDetail hasFocus problemDetail = BL.renderList renderFunc hasFocus codeList
  where
    codeList = codeDefinitionList problemDetail
    codeVector = BL.listElements codeList
    renderFunc = renderLanguage

renderLanguage :: Bool -> (String, String) -> Widget ResourceName
renderLanguage bool = drawStr bool . fst
