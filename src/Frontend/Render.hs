module Frontend.Render where

import Brick
import Brick.Widgets.Border
import qualified Brick.Widgets.Edit as E
import Frontend.Help
import qualified Frontend.Problem as P
import qualified Frontend.ProblemDetail as PD
import Frontend.State
import qualified Frontend.Submission as S
import qualified Frontend.SubmissionDetail as SD
import qualified Frontend.Tab as Tab
import qualified Frontend.UserInfo as UI
import Frontend.Utils

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
      Just message -> drawGreen False message

drawSubmission :: TuiState -> [Widget ResourceName]
drawSubmission ts =
  [ vBox [topWidget, submissionWidget, bottomWidget]
  ]
  where
    currentFocus = tuiStateSubmissionFocus ts
    userInfoWidget = UI.renderUserInfo $ tuiStateUserInfo ts
    -- top
    tabWidget = Tab.renderTab (tuiStateTab ts)
    topWidget = tabWidget <+> userInfoWidget
    -- middle
    submissionListWidget = hLimitPercent 70 $ S.renderSubmission (currentFocus == ListFocus) $ tuiStateSubmissionList ts
    submissionDetailWidget = padBottom Max $ case tuiStateSubmissionDetail ts of
      Nothing -> str " "
      Just submissionDetail -> SD.renderSubmission (currentFocus == DetailFocus) submissionDetail
    rightSide = submissionDetailWidget <=> renderHelp
    submissionWidget = hBox [submissionListWidget, vBorder, rightSide]
    -- bottom
    bottomWidget = case tuiStateMessage ts of
      Nothing -> E.renderEditor (str . unlines) (currentFocus == SearchFocus) (tuiStateSubmissionSearch ts)
      Just message -> drawGreen False message
