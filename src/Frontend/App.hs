{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wunused-imports #-}

module Frontend.App where

import qualified Backend.Problem as P
import Backend.UserInfo (getUserInfo)
import Brick
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as BL
import qualified Data.Vector as V
import Frontend.KeyBinding (handleTuiEvent)
import Frontend.Render (drawTui)
import Frontend.State
import Frontend.Utils (colorMap)

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  return ()

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = return,
      appAttrMap = const $ attrMap mempty colorMap
    }

buildInitialState :: IO TuiState
buildInitialState =
  do
    userInfo <- getUserInfo
    problems <- P.getProblems
    let submissions = V.empty
    let isCurrentUserPremium = snd $ head $ filter (\tup -> fst tup == "premium") userInfo
    let filterProblems = case isCurrentUserPremium of
          "False" -> V.filter (not . P.paidOnly) problems
          _ -> problems
    return $
      TuiState
        { tuiStateUserInfo = userInfo,
          tuiStateProblemList = BL.list DownloadListView filterProblems 1,
          tuiStateDownloadFocus = ListFocus,
          tuiStateProblemDetail = Nothing,
          tuiStateDownloadSearch = E.editor DownloadSearchView (Just 1) "",
          tuiStateMessage = Nothing,
          tuiStateTab = DownloadTab,
          -- Submission
          tuiStateSubmissionFocus = ListFocus,
          tuiStateSubmissionSearch = E.editor SubmissionSearchView (Just 1) "",
          tuiStateSubmissionDetail = Nothing,
          tuiStateSubmissionReport = Nothing,
          tuiStateSubmissionList = BL.list SubmissionListView submissions 1
        }
