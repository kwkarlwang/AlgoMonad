{-# OPTIONS -Wunused-imports #-}
module Frontend.Submission where

import Backend.Submission (Submission (slug), pid)
import Brick (Widget)
import Brick.Widgets.List as BL hiding (reverse)
import Frontend.State (ResourceName)
import Frontend.Utils (drawSelectedOrUnfocus, drawStr)

renderSubmission :: Bool -> BL.List ResourceName Submission -> Widget ResourceName
renderSubmission hasFocus = BL.renderList renderFunc hasFocus
  where
    renderFunc :: Bool -> Submission -> Widget ResourceName
    renderFunc = renderTitle hasFocus

renderTitle :: Bool -> Bool -> Submission -> Widget ResourceName
renderTitle hasFocus isSelected submission = widget
  where
    title = " " ++ showTitle submission
    widget = drawSelectedOrUnfocus isSelected hasFocus drawStr title

showTitle :: Submission -> String
showTitle submission = (show . pid) submission ++ "." ++ slug submission
