{-# OPTIONS -Wunused-imports #-}
module Frontend.Render where

import Brick
import Brick.Widgets.Border
import qualified Brick.Widgets.Edit as E
import qualified Download.ProblemList.Render as P
import Frontend.Help
import qualified Frontend.ProblemDetail as PD
import Frontend.State
import qualified Frontend.Submission as S
import qualified Frontend.SubmissionDetail as SD
import qualified Frontend.SubmissionReport as SR
import qualified Frontend.Tab as Tab
import Frontend.Utils
import qualified UserInfo.Render as UI

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = case tuiStateTab ts of
  DownloadTab -> drawDownload ts
  SubmissionTab -> drawSubmission ts

drawDownload :: TuiState -> [Widget ResourceName]
drawDownload ts =
  [ vBox [topWidget, problemWidget, bottomWidget]
  ]
  where
    currentFocus = tuiStateDownloadFocus ts
    userInfoWidget = UI.renderUserInfo $ tuiStateUserInfo ts
    -- top
    tabWidget = Tab.renderTab (tuiStateTab ts)
    topWidget = tabWidget <+> userInfoWidget
    -- middle
    problemListWidget = hLimitPercent 70 $ P.renderProblem (currentFocus == ListFocus) $ tuiStateProblemList ts
    problemDetailWidget = padBottom Max $ case tuiStateProblemDetail ts of
      Nothing -> str " "
      Just problemDetail -> PD.renderProblemDetail (currentFocus == DetailFocus) problemDetail
    rightSide = problemDetailWidget <=> renderHelp
    problemWidget = hBox [problemListWidget, vBorder, rightSide]
    -- bottom
    bottomWidget = case tuiStateMessage ts of
      Nothing -> E.renderEditor (str . unlines) (currentFocus == SearchFocus) (tuiStateDownloadSearch ts)
      Just message -> drawGreen message

drawSubmission :: TuiState -> [Widget ResourceName]
drawSubmission ts = [vBox [topWidget, submissionWidget, bottomWidget]]
  where
    currentFocus = tuiStateSubmissionFocus ts
    userInfoWidget = UI.renderUserInfo $ tuiStateUserInfo ts
    -- top
    tabWidget = Tab.renderTab (tuiStateTab ts)
    topWidget = tabWidget <+> userInfoWidget
    -- middle
    -- left
    submissionListWidget = vLimitPercent 50 $ S.renderSubmission (currentFocus == ListFocus) $ tuiStateSubmissionList ts
    submissionReportWidget = case tuiStateSubmissionReport ts of
      Nothing -> str ""
      Just submissionReport -> SR.renderSubmissionReport submissionReport
    leftSide = hLimitPercent 70 $ submissionListWidget <=> hBorder <=> submissionReportWidget
    -- right
    submissionDetailWidget = padBottom Max $ case tuiStateSubmissionDetail ts of
      Nothing -> str " "
      Just submissionDetail -> SD.renderSubmission (currentFocus == DetailFocus) submissionDetail
    rightSide = submissionDetailWidget <=> renderHelp
    submissionWidget = hBox [leftSide, vBorder, rightSide]
    -- bottom
    bottomWidget = case tuiStateMessage ts of
      Nothing -> E.renderEditor (str . unlines) (currentFocus == SearchFocus) (tuiStateSubmissionSearch ts)
      Just message -> drawGreen message
