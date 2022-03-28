{-# OPTIONS -Wunused-imports #-}
module Frontend.Submission where

import Backend.Submission (Submission (slug), pid)
import Brick (Widget)
import Brick.Widgets.List as BL hiding (reverse)
import Frontend.State (ResourceName)
import Frontend.Utils (drawStr)

renderSubmission :: Bool -> BL.List ResourceName Submission -> Widget ResourceName
renderSubmission = BL.renderList renderFunc
  where
    renderFunc = renderTitle

renderTitle :: Bool -> Submission -> Widget ResourceName
renderTitle bool submission = widget
  where
    title = " " ++ (show . pid) submission ++ "." ++ slug submission
    widget = drawStr bool title
