{-# OPTIONS -Wunused-imports #-}
module Frontend.ProblemDetail where

import Brick (Widget)
import Brick.Widgets.List as BL hiding (reverse)
import Frontend.State (ProblemDetailList (codeDefinitionList), ResourceName)
import Frontend.Utils (drawStr)

renderProblemDetail :: Bool -> ProblemDetailList -> Widget ResourceName
renderProblemDetail hasFocus problemDetail = BL.renderList renderFunc hasFocus codeList
  where
    codeList = codeDefinitionList problemDetail
    codeVector = BL.listElements codeList
    renderFunc = renderLanguage

renderLanguage :: Bool -> (String, String) -> Widget ResourceName
renderLanguage bool = drawStr bool . fst
