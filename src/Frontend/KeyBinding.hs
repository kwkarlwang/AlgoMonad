module Frontend.KeyBinding where

import Backend.Problem (Problem (pid))
import qualified Backend.Problem as P
import qualified Backend.ProblemDetail as PD
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
import Data.Char (toLower)
import Data.List (isInfixOf)
import Frontend.Problem (showTitle)
import Frontend.State
  ( Focus (DetailFocus, ProblemFocus, SearchFocus),
    NewState,
    ProblemDetailList (ProblemDetailList, codeDefinitionList, content, slug),
    ResourceName (DetailView, ProblemView, SearchView),
    TuiState
      ( TuiState,
        tuiStateCurrentFocus,
        tuiStateProblemDetail,
        tuiStateProblemList,
        tuiStateProblems,
        tuiStateSearch,
        tuiStateUserInfo
      ),
  )
import Graphics.Vty.Input.Events

emptyEditor = E.editor SearchView (Just 1) ""

handleFocusProblem :: TuiState -> EventM ResourceName TuiState
handleFocusProblem s = return s {tuiStateCurrentFocus = ProblemFocus, tuiStateProblemDetail = Nothing}

handleFocusDetail :: TuiState -> EventM ResourceName TuiState
handleFocusDetail s = do
  let maybeProblem = BL.listSelectedElement $ tuiStateProblemList s
  case maybeProblem of
    Nothing -> return s
    Just (idx, problem) -> do
      problemDetail <- liftIO $ PD.getProblemDetail $ P.slug problem
      let problemDetailList =
            ProblemDetailList
              { slug = PD.slug problemDetail,
                content = PD.content problemDetail,
                codeDefinitionList = BL.list DetailView (PD.codeDefinitionVector problemDetail) 1
              }
      return s {tuiStateCurrentFocus = DetailFocus, tuiStateProblemDetail = Just problemDetailList}

handleSearchForward :: TuiState -> EventM ResourceName TuiState
handleSearchForward s = do
  let newSearch = emptyEditor
  newSearchMoveCursor <- E.handleEditorEvent (EvKey (KChar '/') []) newSearch
  return s {tuiStateCurrentFocus = SearchFocus, tuiStateSearch = newSearchMoveCursor}

handleProblemList :: TuiState -> Event -> EventM ResourceName TuiState
handleProblemList s e = do
  let currentFocus = tuiStateCurrentFocus s
  let oldProblemList = tuiStateProblemList s
  newProblemList <- BL.handleListEventVi (\_ l -> return l) e oldProblemList
  return s {tuiStateProblemList = newProblemList}

handleProblemDetail :: TuiState -> Event -> EventM ResourceName TuiState
handleProblemDetail s e = do
  let currentFocus = tuiStateCurrentFocus s
  let oldProblemDetail = tuiStateProblemDetail s
  case (oldProblemDetail, currentFocus) of
    (Just oldProblemDetail, DetailFocus) -> do
      let oldCodeDefinitionList = codeDefinitionList oldProblemDetail
      newCodeDefinitionList <- BL.handleListEventVi (\_ l -> return l) e oldCodeDefinitionList
      let newProblemDetail = ProblemDetailList {slug = slug oldProblemDetail, content = content oldProblemDetail, codeDefinitionList = newCodeDefinitionList}
      return $ s {tuiStateProblemDetail = Just newProblemDetail}
    _ -> return s

handleEvent :: TuiState -> Focus -> Event -> NewState
handleEvent s ProblemFocus (EvKey (KChar 'q') []) = halt s
handleEvent s ProblemFocus (EvKey (KChar 'l') []) = do
  newState <- handleFocusDetail s
  continue newState
handleEvent s ProblemFocus e@(EvKey (KChar '/') []) = do
  newState <- handleSearchForward s
  continue newState
handleEvent s ProblemFocus e = do
  newState <- handleProblemList s e
  continue newState
handleEvent s DetailFocus (EvKey (KChar 'q') []) = halt s
handleEvent s DetailFocus (EvKey (KChar 'h') []) = do
  newState <- handleFocusProblem s
  continue newState
handleEvent s DetailFocus (EvKey KEnter []) = do
  let problemDetail = tuiStateProblemDetail s
  case problemDetail of
    Nothing -> continue s
    Just problemDetail -> do
      let currentCodePair = BL.listSelectedElement $ codeDefinitionList problemDetail
      case currentCodePair of
        Nothing -> continue s
        Just (_, currentCodePair) -> do
          liftIO $ PD.writeProblemToFile (slug problemDetail) (content problemDetail) currentCodePair
          continue s
handleEvent s DetailFocus e = do
  newState <- handleProblemDetail s e
  continue newState
handleEvent s SearchFocus (EvKey KEnter []) = do
  let search = tuiStateSearch s
  let problemList = tuiStateProblemList s
  let content = map toLower $ tail $ head $ E.getEditContents search
  let filterCondition p = content `isInfixOf` map toLower (showTitle p)
  let newProblemList = BL.listFindBy filterCondition problemList
  continue s {tuiStateCurrentFocus = ProblemFocus, tuiStateProblemList = newProblemList}
handleEvent s SearchFocus (EvKey KEsc []) = do
  continue s {tuiStateCurrentFocus = ProblemFocus, tuiStateSearch = emptyEditor}
handleEvent s SearchFocus e = do
  let oldSearch = tuiStateSearch s
  newSearch <- E.handleEditorEvent e oldSearch
  continue s {tuiStateSearch = newSearch}

handleTuiEvent :: TuiState -> BrickEvent n e -> NewState
handleTuiEvent s e =
  case e of
    VtyEvent vtye -> do
      let currentFocus = tuiStateCurrentFocus s
      handleEvent s currentFocus vtye
    _ -> continue s
