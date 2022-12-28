{-# OPTIONS -Wunused-imports #-}
module Frontend.KeyBinding where

import qualified Backend.ProblemDetail as PD
import qualified Backend.Submission as S
import qualified Backend.SubmissionDetail as SD
import Backend.Utils (getProblemAddress)
import Brick
  ( BrickEvent (VtyEvent),
    EventM,
    continue,
    halt,
  )
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.List (List)
import qualified Brick.Widgets.List as BL
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isDigit, isUpper, toLower)
import Data.List (isInfixOf)
import qualified Data.Vector as V
import Download.ProblemList.Render (showTitle)
import Download.ProblemList.Request (getProblems)
import qualified Download.ProblemList.State as P
import Frontend.State
import qualified Frontend.Submission as FS
import Graphics.Vty.Input.Events
import UserInfo.Request (getUserInfo)
import UserInfo.State (UserInfo (premium))
import Web.Browser (openBrowser)

emptyEditor = E.editor DownloadSearchView (Just 1) ""

getSearch :: Tab -> TuiState -> E.Editor String ResourceName
getSearch DownloadTab = tuiStateDownloadSearch
getSearch SubmissionTab = tuiStateSubmissionSearch

newListWithIndex :: List ResourceName a -> V.Vector a -> List ResourceName a
newListWithIndex oldList vector = newList
  where
    newInitialList = BL.list (BL.listName oldList) vector 1
    newList = case BL.listSelected oldList of
      Just idx -> BL.listMoveTo idx newInitialList
      Nothing -> newInitialList

handleSwitching :: TuiState -> Tab -> Focus -> EventM ResourceName TuiState
handleSwitching s DownloadTab ListFocus = do
  let maybeProblem = BL.listSelectedElement $ tuiStateProblemList s
  case maybeProblem of
    Nothing -> return s
    Just (_, problem) -> do
      let targetPid = P.pid problem
      intermediateState <- handleGetSubmissions s
      let oldList = tuiStateSubmissionList intermediateState
      let newList = BL.listFindBy (\submission -> S.pid submission == targetPid) oldList
      if Just targetPid == (S.pid . snd <$> BL.listSelectedElement newList)
        then return intermediateState {tuiStateSubmissionList = newList, tuiStateTab = SubmissionTab}
        else return s {tuiStateMessage = Just ("Cannot find problem " ++ show targetPid ++ " in current folder")}
handleSwitching s DownloadTab DetailFocus = do
  newState <- handleSwitching s DownloadTab ListFocus
  handleFocusSubmissionDetail newState
handleSwitching s _ _ = undefined

handleGetSubmissions :: TuiState -> EventM ResourceName TuiState
handleGetSubmissions s = do
  submissions <- liftIO S.getSubmissions
  let oldList = tuiStateSubmissionList s
  return s {tuiStateSubmissionList = newListWithIndex oldList submissions}

handleRefresh :: TuiState -> EventM ResourceName TuiState
handleRefresh s = do
  userInfo <- liftIO getUserInfo
  problems <- liftIO getProblems
  let isCurrentUserPremium = premium userInfo
  let filterProblems =
        if isCurrentUserPremium
          then problems
          else V.filter (not . P.paidOnly) problems
  let oldList = tuiStateProblemList s
  return
    s
      { tuiStateProblemList = newListWithIndex oldList problems,
        tuiStateUserInfo = userInfo,
        tuiStateMessage = Just "Refresh successfully!"
      }

handleSearch :: TuiState -> Tab -> EventM ResourceName TuiState
handleSearch s DownloadTab = do
  let search = tuiStateDownloadSearch s
  let problemList = tuiStateProblemList s
  let searchText = head $ E.getEditContents search
  let filterCondition p = case (head searchText, tail searchText) of
        (_, "") -> do
          False
        (':', searchText) -> do
          let problemId = read searchText :: Integer
          problemId == P.pid p
        (_, searchText) -> do
          let content = map toLower searchText
          content `isInfixOf` map toLower (showTitle p)
  let newProblemList = BL.listFindBy filterCondition problemList
  return s {tuiStateProblemList = newProblemList}
handleSearch s SubmissionTab = do
  let search = tuiStateSubmissionSearch s
  let submissionList = tuiStateSubmissionList s
  let searchText = head $ E.getEditContents search
  let filterCondition s = case (head searchText, tail searchText) of
        (_, "") -> do
          False
        (':', searchText) -> do
          let problemId = read searchText :: Integer
          problemId == S.pid s
        (_, searchText) -> do
          let content = map toLower searchText
          content `isInfixOf` map toLower (FS.showTitle s)
  let newSubmissionList = BL.listFindBy filterCondition submissionList
  return s {tuiStateSubmissionList = newSubmissionList}

handleFocusProblem :: TuiState -> EventM ResourceName TuiState
handleFocusProblem s = return s {tuiStateDownloadFocus = ListFocus, tuiStateProblemDetail = Nothing}

handleFocusProblemDetail :: TuiState -> EventM ResourceName TuiState
handleFocusProblemDetail s = do
  let maybeProblem = BL.listSelectedElement $ tuiStateProblemList s
  case maybeProblem of
    Nothing -> return s
    Just (_, problem) -> do
      problemDetail <- liftIO $ PD.getProblemDetail (P.slug problem) (P.pid problem)
      let problemDetailList =
            ProblemDetailList
              { pid = PD.pid problemDetail,
                slug = PD.slug problemDetail,
                content = PD.content problemDetail,
                codeSnippets = BL.list DownloadDetailView (PD.codeDefinitionVector problemDetail) 1,
                likes = PD.likes problemDetail,
                dislikes = PD.dislikes problemDetail
              }
      return s {tuiStateDownloadFocus = DetailFocus, tuiStateProblemDetail = Just problemDetailList}

handleSearchInput :: TuiState -> Event -> EventM ResourceName TuiState
handleSearchInput s e = do
  let newSearch = emptyEditor
  newSearchMoveCursor <- E.handleEditorEvent e newSearch
  case tuiStateTab s of
    DownloadTab -> return s {tuiStateDownloadFocus = SearchFocus, tuiStateDownloadSearch = newSearchMoveCursor}
    SubmissionTab -> return s {tuiStateSubmissionFocus = SearchFocus, tuiStateSubmissionSearch = newSearchMoveCursor}

handleProblemList :: TuiState -> Event -> EventM ResourceName TuiState
handleProblemList s e = do
  let oldProblemList = tuiStateProblemList s
  newProblemList <- BL.handleListEventVi (\_ l -> return l) e oldProblemList
  return s {tuiStateProblemList = newProblemList}

handleProblemDetail :: TuiState -> Event -> EventM ResourceName TuiState
handleProblemDetail s e@(EvKey (KChar char) []) = do
  let currentFocus = tuiStateDownloadFocus s
  let oldProblemDetail = tuiStateProblemDetail s
  case (oldProblemDetail, currentFocus, isUpper char) of
    (Just oldProblemDetail, DetailFocus, True) -> do
      let oldCodeDefinitionList = codeSnippets oldProblemDetail
      let newCodeDefinitionList = BL.listFindBy (\tup -> (toLower . head) (fst tup) == toLower char) oldCodeDefinitionList
      let newProblemDetail =
            ProblemDetailList
              { slug = slug oldProblemDetail,
                content = content oldProblemDetail,
                codeSnippets = newCodeDefinitionList,
                pid = pid oldProblemDetail,
                likes = likes oldProblemDetail,
                dislikes = dislikes oldProblemDetail
              }
      return $ s {tuiStateProblemDetail = Just newProblemDetail}
    (Just oldProblemDetail, DetailFocus, False) -> do
      let oldCodeDefinitionList = codeSnippets oldProblemDetail
      newCodeDefinitionList <- BL.handleListEventVi (\_ l -> return l) e oldCodeDefinitionList
      let newProblemDetail =
            ProblemDetailList
              { slug = slug oldProblemDetail,
                content = content oldProblemDetail,
                codeSnippets = newCodeDefinitionList,
                pid = pid oldProblemDetail,
                likes = likes oldProblemDetail,
                dislikes = dislikes oldProblemDetail
              }
      return $ s {tuiStateProblemDetail = Just newProblemDetail}
    _ -> return s
handleProblemDetail s e = do
  let currentFocus = tuiStateDownloadFocus s
  let oldProblemDetail = tuiStateProblemDetail s
  case (oldProblemDetail, currentFocus) of
    (Just oldProblemDetail, DetailFocus) -> do
      let oldCodeDefinitionList = codeSnippets oldProblemDetail
      newCodeDefinitionList <- BL.handleListEventVi (\_ l -> return l) e oldCodeDefinitionList
      let newProblemDetail =
            ProblemDetailList
              { slug = slug oldProblemDetail,
                content = content oldProblemDetail,
                codeSnippets = newCodeDefinitionList,
                pid = pid oldProblemDetail,
                likes = likes oldProblemDetail,
                dislikes = dislikes oldProblemDetail
              }
      return $ s {tuiStateProblemDetail = Just newProblemDetail}
    _ -> return s

handleFocusSubmission :: TuiState -> EventM ResourceName TuiState
handleFocusSubmission s = return s {tuiStateSubmissionFocus = ListFocus, tuiStateSubmissionDetail = Nothing}

handleFocusSubmissionDetail :: TuiState -> EventM ResourceName TuiState
handleFocusSubmissionDetail s = do
  let maybeSubmission = BL.listSelectedElement $ tuiStateSubmissionList s
  case maybeSubmission of
    Nothing -> return s
    Just (_, submission) -> do
      let submissionDetailList = BL.list SubmissionDetailView (S.langPaths submission) 1
      return s {tuiStateSubmissionFocus = DetailFocus, tuiStateSubmissionDetail = Just submissionDetailList}

handleSubmissionList :: TuiState -> Event -> EventM ResourceName TuiState
handleSubmissionList s e = do
  let oldSubmissionList = tuiStateSubmissionList s
  newSubmissionList <- BL.handleListEventVi (\_ l -> return l) e oldSubmissionList
  return s {tuiStateSubmissionList = newSubmissionList}

handleSubmissionDetail :: TuiState -> Event -> EventM ResourceName TuiState
handleSubmissionDetail s e = do
  -- WARNING: this line might not be necessary
  let currentFocus = tuiStateSubmissionFocus s
  let oldSubmissionDetail = tuiStateSubmissionDetail s
  case (oldSubmissionDetail, currentFocus) of
    (Just oldSubmissionDetail, DetailFocus) -> do
      newSubmissionDetail <- BL.handleListEventVi (\_ l -> return l) e oldSubmissionDetail
      return $ s {tuiStateSubmissionDetail = Just newSubmissionDetail}
    _ -> return s

handleSubmissionReport :: TuiState -> EventM ResourceName TuiState
handleSubmissionReport s = do
  let submissionDetail = BL.listSelectedElement <$> tuiStateSubmissionDetail s
  let submission = BL.listSelectedElement $ tuiStateSubmissionList s

  case (submission, submissionDetail) of
    (Just (_, submission), Just (Just (_, filepath))) -> do
      let problemList = BL.listElements $ tuiStateProblemList s
      let problem = V.head $ V.filter (\problem -> P.pid problem == S.pid submission) problemList
      submissionReport <- liftIO $ SD.getSubmissionReport filepath (S.slug submission) (S.pid submission) (P.submitPid problem)
      return s {tuiStateSubmissionReport = Just submissionReport}
    _ -> return s

handleOpenProblemInDownload :: TuiState -> EventM ResourceName TuiState
handleOpenProblemInDownload s = do
  let currentProblemPair = BL.listSelectedElement $ tuiStateProblemList s
  case currentProblemPair of
    Nothing -> return s
    Just (_, currentProblem) -> do
      success <- liftIO $ openBrowser $ getProblemAddress $ P.slug currentProblem
      if success
        then return s {tuiStateMessage = Just "Open website successfully!"}
        else return s {tuiStateMessage = Just "Open website failed!"}

handleOpenProblemInSubmission :: TuiState -> EventM ResourceName TuiState
handleOpenProblemInSubmission s = do
  let currentProblemPair = BL.listSelectedElement $ tuiStateSubmissionList s
  case currentProblemPair of
    Nothing -> return s
    Just (_, currentProblem) -> do
      success <- liftIO $ openBrowser $ getProblemAddress $ S.slug currentProblem
      if success
        then return s {tuiStateMessage = Just "Open website successfully!"}
        else return s {tuiStateMessage = Just "Open website failed!"}

handleEvent :: TuiState -> Tab -> Focus -> Event -> NewState
-- Download
-- List
handleEvent s _ ListFocus (EvKey (KChar 'r') []) = handleRefresh s >>= continue
handleEvent s _ ListFocus (EvKey (KChar 'q') []) = halt s
handleEvent s DownloadTab ListFocus (EvKey (KChar 'l') []) = handleFocusProblemDetail s >>= continue
handleEvent s _ ListFocus e@(EvKey (KChar '/') []) = handleSearchInput s e >>= continue
handleEvent s _ ListFocus e@(EvKey (KChar ':') []) = handleSearchInput s e >>= continue
handleEvent s tab focus (EvKey (KChar 's') []) = handleSwitching s tab focus >>= continue
-- Search next occurance
handleEvent s tab ListFocus e@(EvKey (KChar 'n') []) = handleSearch s tab >>= continue
handleEvent s DownloadTab ListFocus (EvKey (KChar '2') []) = do
  newState <- handleGetSubmissions s
  continue newState {tuiStateTab = SubmissionTab}

-- Open question in browser
handleEvent s DownloadTab ListFocus (EvKey (KChar 'o') []) = handleOpenProblemInDownload s >>= continue
handleEvent s DownloadTab ListFocus e = handleProblemList s e >>= continue
-- Detail
handleEvent s _ DetailFocus (EvKey (KChar 'r') []) = handleRefresh s >>= continue
handleEvent s _ DetailFocus (EvKey (KChar 'q') []) = halt s
handleEvent s DownloadTab DetailFocus (EvKey (KChar 'h') []) = handleFocusProblem s >>= continue
-- download question
handleEvent s DownloadTab DetailFocus (EvKey KEnter []) = do
  let problemDetail = tuiStateProblemDetail s
  case problemDetail of
    Nothing -> continue s
    Just problemDetail -> do
      let currentCodePair = BL.listSelectedElement $ codeSnippets problemDetail
      case currentCodePair of
        Nothing -> continue s
        Just (_, currentCodePair) -> do
          liftIO $ PD.writeProblemToFile (slug problemDetail) (content problemDetail) currentCodePair (pid problemDetail)
          continue s {tuiStateMessage = Just "Download successfully!"}
handleEvent s DownloadTab DetailFocus (EvKey (KChar '2') []) = do
  submissions <- liftIO S.getSubmissions
  let oldList = tuiStateSubmissionList s
  let newList = BL.list SubmissionListView submissions 1
  let idx = BL.listSelected oldList
  continue
    s
      { tuiStateTab = SubmissionTab,
        tuiStateSubmissionList =
          case idx of
            Just idx -> BL.listMoveTo idx newList
            Nothing -> newList
      }
handleEvent s DownloadTab DetailFocus (EvKey (KChar 'o') []) = handleOpenProblemInDownload s >>= continue
handleEvent s DownloadTab DetailFocus e = handleProblemDetail s e >>= continue
-- Search
handleEvent s tab SearchFocus (EvKey KEnter []) = do
  newState <- handleSearch s tab
  case tab of
    DownloadTab -> continue newState {tuiStateDownloadFocus = ListFocus}
    SubmissionTab -> continue newState {tuiStateSubmissionFocus = ListFocus}
handleEvent s DownloadTab SearchFocus (EvKey KEsc []) = do
  continue s {tuiStateDownloadFocus = ListFocus, tuiStateDownloadSearch = emptyEditor}
handleEvent s tab SearchFocus e@(EvKey KBS []) = do
  let oldSearch = getSearch tab s
  newSearch <- E.handleEditorEvent e oldSearch
  let content = head $ E.getEditContents newSearch
  let focus = if null content then ListFocus else SearchFocus
  case tab of
    DownloadTab -> continue s {tuiStateDownloadSearch = newSearch, tuiStateDownloadFocus = focus}
    SubmissionTab -> continue s {tuiStateSubmissionSearch = newSearch, tuiStateSubmissionFocus = focus}
handleEvent s tab SearchFocus e@(EvKey (KChar char) _) = do
  let oldSearch = getSearch tab s
  let content = head $ E.getEditContents oldSearch
  newSearch <- case (head content, isDigit char) of
    (':', True) -> E.handleEditorEvent e oldSearch
    ('/', _) -> E.handleEditorEvent e oldSearch
    (_, _) -> return oldSearch
  case tab of
    DownloadTab -> continue s {tuiStateDownloadSearch = newSearch}
    SubmissionTab -> continue s {tuiStateSubmissionSearch = newSearch}
-- Submission
-- List
handleEvent s SubmissionTab ListFocus (EvKey (KChar 'l') []) = handleFocusSubmissionDetail s >>= continue
-- not needed as of now, refresh metadata more important
-- handleEvent s SubmissionTab ListFocus (EvKey (KChar 'r') []) = do
--   submissions <- liftIO S.getSubmissions
--   let oldList = tuiStateSubmissionList s
--   let newList = BL.list SubmissionListView submissions 1
--   let idx = BL.listSelected oldList
--   continue
--     s
--       { tuiStateSubmissionList =
--           case idx of
--             Just idx -> BL.listMoveTo idx newList
--             Nothing -> newList
--       }
handleEvent s SubmissionTab ListFocus (EvKey (KChar 'o') []) = do
  newState <- handleOpenProblemInSubmission s
  continue newState
handleEvent s SubmissionTab ListFocus (EvKey (KChar '1') []) = continue s {tuiStateTab = DownloadTab}
handleEvent s SubmissionTab ListFocus e = handleSubmissionList s e >>= continue
-- Detail
handleEvent s SubmissionTab DetailFocus (EvKey (KChar 'h') []) = handleFocusSubmission s >>= continue
handleEvent s SubmissionTab DetailFocus (EvKey KEnter []) = do
  newState <- handleSubmissionReport s
  continue newState {tuiStateMessage = Just "Submit successfully!"}

-- Open question in browser
handleEvent s SubmissionTab DetailFocus (EvKey (KChar 'o') []) = do
  newState <- handleOpenProblemInSubmission s
  continue newState
handleEvent s SubmissionTab DetailFocus (EvKey (KChar '1') []) = continue s {tuiStateTab = DownloadTab}
handleEvent s SubmissionTab DetailFocus e = handleSubmissionDetail s e >>= continue
handleEvent s SubmissionTab SearchFocus (EvKey KEsc []) =
  continue s {tuiStateSubmissionFocus = ListFocus, tuiStateSubmissionSearch = emptyEditor}
-- all other case
handleEvent s tab SearchFocus e = do
  let oldSearch = getSearch tab s
  newSearch <- E.handleEditorEvent e oldSearch
  case tab of
    DownloadTab -> continue s {tuiStateDownloadSearch = newSearch}
    SubmissionTab -> continue s {tuiStateSubmissionSearch = newSearch}

handleTuiEvent :: TuiState -> BrickEvent n e -> NewState
handleTuiEvent s e =
  case e of
    VtyEvent vtye -> do
      let currentTab = tuiStateTab s
      let currentFocus = (if currentTab == DownloadTab then tuiStateDownloadFocus else tuiStateSubmissionFocus) s
      let s1 = s {tuiStateMessage = Nothing}
      handleEvent s1 currentTab currentFocus vtye
    _ -> continue s
