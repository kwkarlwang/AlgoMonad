{-# OPTIONS -Wunused-imports #-}
module Frontend.Submission where

import Backend.Submission (Submission (slug), pid)
import Brick (Widget)
import Brick.Widgets.List as BL hiding (reverse)
import Frontend.State (ResourceName)
import Frontend.Utils (drawSelected, drawStr)

renderSubmission :: Bool -> BL.List ResourceName Submission -> Widget ResourceName
renderSubmission = BL.renderList renderFunc
  where
    renderFunc = renderTitle

renderTitle :: Bool -> Submission -> Widget ResourceName
renderTitle isSelected submission = widget
  where
    title = " " ++ showTitle submission
    widget = drawSelected isSelected drawStr title

showTitle :: Submission -> String
showTitle submission = (show . pid) submission ++ "." ++ slug submission
