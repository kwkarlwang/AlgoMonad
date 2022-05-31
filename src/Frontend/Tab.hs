{-# OPTIONS -Wunused-imports #-}
module Frontend.Tab where

import Brick (Widget, hBox, hLimitPercent)
import Frontend.State (ResourceName, Tab (DownloadTab, SubmissionTab))
import Frontend.Utils (drawStr, drawTab)

renderTab :: Tab -> Widget ResourceName
renderTab tab =
  hLimitPercent 30 $
    hBox
      [ drawStr " ",
        drawTab (tab == DownloadTab) "   1 Download   ",
        drawStr " ",
        drawTab (tab == SubmissionTab) "  2 Submission  ",
        drawStr " "
      ]
