module Frontend.Tab where

import Brick (Widget, hBox, hLimitPercent, setAvailableSize)
import Brick.Widgets.Center (hCenter, hCenterWith)
import Frontend.State (ResourceName, Tab (DownloadTab, SubmissionTab))
import Frontend.Utils (drawStr, drawTab)

renderTab :: Tab -> Widget ResourceName
renderTab tab =
  hLimitPercent 30 $
    hBox
      [ drawStr False " ",
        drawTab (tab == DownloadTab) "   1 Download   ",
        drawStr False " ",
        drawTab (tab == SubmissionTab) "  2 Submission  ",
        drawStr False " "
      ]
