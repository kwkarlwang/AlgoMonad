module Frontend.KeyBinding where

import qualified Backend.Problem as P
import qualified Backend.ProblemDetail as PD
import Backend.Submission (langPaths)
import Brick
  ( BrickEvent (VtyEvent),
    EventM,
    continue,
    hBox,
    halt,
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as BL
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isDigit, isUpper, toLower)
import Data.List (isInfixOf)
import Debug.Trace (traceM)
import Frontend.Problem (showTitle)
import Frontend.State
import Graphics.Vty.Input.Events

emptyEditor = E.editor DownloadSearchView (Just 1) ""

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
                codeDefinitionList = BL.list DownloadDetailView (PD.codeDefinitionVector problemDetail) 1
              }
      return s {tuiStateDownloadFocus = DetailFocus, tuiStateProblemDetail = Just problemDetailList}

handleSearch :: TuiState -> Event -> EventM ResourceName TuiState
handleSearch s e = do
  let newSearch = emptyEditor
  newSearchMoveCursor <- E.handleEditorEvent e newSearch
  return s {tuiStateDownloadFocus = SearchFocus, tuiStateDownloadSearch = newSearchMoveCursor}

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
      let oldCodeDefinitionList = codeDefinitionList oldProblemDetail
      let newCodeDefinitionList = BL.listFindBy (\tup -> (toLower . head) (fst tup) == toLower char) oldCodeDefinitionList
      let newProblemDetail =
            ProblemDetailList
              { slug = slug oldProblemDetail,
                content = content oldProblemDetail,
                codeDefinitionList = newCodeDefinitionList,
                pid = pid oldProblemDetail
              }
      return $ s {tuiStateProblemDetail = Just newProblemDetail}
    (Just oldProblemDetail, DetailFocus, False) -> do
      let oldCodeDefinitionList = codeDefinitionList oldProblemDetail
      newCodeDefinitionList <- BL.handleListEventVi (\_ l -> return l) e oldCodeDefinitionList
      let newProblemDetail =
            ProblemDetailList
              { slug = slug oldProblemDetail,
                content = content oldProblemDetail,
                codeDefinitionList = newCodeDefinitionList,
                pid = pid oldProblemDetail
              }
      return $ s {tuiStateProblemDetail = Just newProblemDetail}
    _ -> return s
handleProblemDetail s e = do
  let currentFocus = tuiStateDownloadFocus s
  let oldProblemDetail = tuiStateProblemDetail s
  case (oldProblemDetail, currentFocus) of
    (Just oldProblemDetail, DetailFocus) -> do
      let oldCodeDefinitionList = codeDefinitionList oldProblemDetail
      newCodeDefinitionList <- BL.handleListEventVi (\_ l -> return l) e oldCodeDefinitionList
      let newProblemDetail =
            ProblemDetailList
              { slug = slug oldProblemDetail,
                content = content oldProblemDetail,
                codeDefinitionList = newCodeDefinitionList,
                pid = pid oldProblemDetail
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
      let submissionDetailList = BL.list SubmissionDetailView (langPaths submission) 1
      return s {tuiStateSubmissionFocus = DetailFocus, tuiStateSubmissionDetail = Just submissionDetailList}

handleSubmissionList :: TuiState -> Event -> EventM ResourceName TuiState
handleSubmissionList s e = do
  let oldSubmissionList = tuiStateSubmissionList s
  newSubmissionList <- BL.handleListEventVi (\_ l -> return l) e oldSubmissionList
  return s {tuiStateSubmissionList = newSubmissionList}

handleSubmissionDetail s e = do
  let currentFocus = tuiStateSubmissionFocus s
  let oldSubmissionDetail = tuiStateSubmissionDetail s
  case (oldSubmissionDetail, currentFocus) of
    (Just oldSubmissionDetail, DetailFocus) -> do
      newSubmissionDetail <- BL.handleListEventVi (\_ l -> return l) e oldSubmissionDetail
      return $ s {tuiStateSubmissionDetail = Just newSubmissionDetail}
    _ -> return s

handleEvent :: TuiState -> Tab -> Focus -> Event -> NewState
-- Download
-- List
handleEvent s _ ListFocus (EvKey (KChar 'q') []) = halt s
handleEvent s DownloadTab ListFocus (EvKey (KChar 'l') []) = do
  newState <- handleFocusProblemDetail s
  continue newState
handleEvent s DownloadTab ListFocus e@(EvKey (KChar '/') []) = do
  newState <- handleSearch s e
  continue newState
handleEvent s DownloadTab ListFocus e@(EvKey (KChar '=') []) = do
  newState <- handleSearch s e
  continue newState
handleEvent s DownloadTab ListFocus e@(EvKey (KChar 'n') []) = do
  let search = tuiStateDownloadSearch s
  let problemList = tuiStateProblemList s
  let searchText = head $ E.getEditContents search
  let filterCondition p = case head searchText of
        '=' -> do
          let problemId = read $ tail searchText :: Integer
          problemId == P.pid p
        _ -> do
          let content = map toLower $ tail searchText
          content `isInfixOf` map toLower (showTitle p)
  let newProblemList = BL.listFindBy filterCondition problemList
  continue s {tuiStateProblemList = newProblemList}
handleEvent s DownloadTab ListFocus (EvKey (KChar '2') []) = continue s {tuiStateTab = SubmissionTab}
handleEvent s DownloadTab ListFocus e = do
  newState <- handleProblemList s e
  continue newState
-- Detail
handleEvent s _ DetailFocus (EvKey (KChar 'q') []) = halt s
handleEvent s DownloadTab DetailFocus (EvKey (KChar 'h') []) = do
  newState <- handleFocusProblem s
  continue newState
handleEvent s DownloadTab DetailFocus (EvKey KEnter []) = do
  let problemDetail = tuiStateProblemDetail s
  case problemDetail of
    Nothing -> continue s
    Just problemDetail -> do
      let currentCodePair = BL.listSelectedElement $ codeDefinitionList problemDetail
      case currentCodePair of
        Nothing -> continue s
        Just (_, currentCodePair) -> do
          liftIO $ PD.writeProblemToFile (slug problemDetail) (content problemDetail) currentCodePair (pid problemDetail)
          continue s {tuiStateMessage = Just "Download successful!"}
handleEvent s DownloadTab DetailFocus (EvKey (KChar '2') []) = continue s {tuiStateTab = SubmissionTab}
handleEvent s DownloadTab DetailFocus e = do
  newState <- handleProblemDetail s e
  continue newState
-- Search
handleEvent s DownloadTab SearchFocus (EvKey KEnter []) = do
  let search = tuiStateDownloadSearch s
  let problemList = tuiStateProblemList s
  let searchText = head $ E.getEditContents search
  let filterCondition p = case (head searchText, tail searchText) of
        (_, "") -> do
          False
        ('=', searchText) -> do
          let problemId = read searchText :: Integer
          problemId == P.pid p
        (_, searchText) -> do
          let content = map toLower searchText
          content `isInfixOf` map toLower (showTitle p)
  let newProblemList = BL.listFindBy filterCondition problemList
  continue s {tuiStateDownloadFocus = ListFocus, tuiStateProblemList = newProblemList}
handleEvent s DownloadTab SearchFocus (EvKey KEsc []) = do
  continue s {tuiStateDownloadFocus = ListFocus, tuiStateDownloadSearch = emptyEditor}
handleEvent s DownloadTab SearchFocus e@(EvKey KBS []) = do
  let oldSearch = tuiStateDownloadSearch s
  newSearch <- E.handleEditorEvent e oldSearch
  let content = head $ E.getEditContents newSearch
  let focus = if null content then ListFocus else SearchFocus
  continue s {tuiStateDownloadSearch = newSearch, tuiStateDownloadFocus = focus}
handleEvent s DownloadTab SearchFocus e@(EvKey (KChar char) _) = do
  let oldSearch = tuiStateDownloadSearch s
  let content = head $ E.getEditContents oldSearch
  newSearch <- case (head content, isDigit char) of
    ('=', True) -> E.handleEditorEvent e oldSearch
    ('/', _) -> E.handleEditorEvent e oldSearch
    (_, _) -> return oldSearch

  continue s {tuiStateDownloadSearch = newSearch}
handleEvent s DownloadTab SearchFocus e = do
  let oldSearch = tuiStateDownloadSearch s
  newSearch <- E.handleEditorEvent e oldSearch
  continue s {tuiStateDownloadSearch = newSearch}
-- Submission
-- List
handleEvent s SubmissionTab ListFocus (EvKey (KChar '1') []) = continue s {tuiStateTab = DownloadTab}
handleEvent s SubmissionTab ListFocus (EvKey (KChar 'l') []) = do
  newState <- handleFocusSubmissionDetail s
  continue newState
handleEvent s SubmissionTab ListFocus e = do
  newState <- handleSubmissionList s e
  continue newState
-- Detail
handleEvent s SubmissionTab DetailFocus (EvKey (KChar '1') []) = continue s {tuiStateTab = DownloadTab}
handleEvent s SubmissionTab DetailFocus (EvKey (KChar 'h') []) = do
  newState <- handleFocusSubmission s
  continue newState
handleEvent s SubmissionTab DetailFocus e = do
  newState <- handleSubmissionDetail s e
  continue newState
handleEvent s _ _ _ = continue s

handleTuiEvent :: TuiState -> BrickEvent n e -> NewState
handleTuiEvent s e =
  case e of
    VtyEvent vtye -> do
      let currentTab = tuiStateTab s
      let currentFocus = (if currentTab == DownloadTab then tuiStateDownloadFocus else tuiStateSubmissionFocus) s
      let s1 = s {tuiStateMessage = Nothing}
      handleEvent s1 currentTab currentFocus vtye
    _ -> continue s
