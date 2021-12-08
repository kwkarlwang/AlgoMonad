module Frontend.KeyBinding where

import qualified Backend.Problem as P
import qualified Backend.ProblemDetail as PD
import Brick
  ( BrickEvent (VtyEvent),
    EventM,
    continue,
    hBox,
    halt,
  )
import Brick.Widgets.List (handleListEventVi)
import qualified Brick.Widgets.List as BL
import Control.Monad.IO.Class (MonadIO (liftIO))
import Frontend.State
  ( Focus (DetailFocus, ProblemFocus),
    NewState,
    ProblemDetailList (ProblemDetailList, codeDefinitionList, content, slug),
    ResourceName (DetailView, ProblemView),
    TuiState
      ( TuiState,
        tuiStateCurrentFocus,
        tuiStateProblemDetail,
        tuiStateProblemList,
        tuiStateProblems,
        tuiStateUserInfo
      ),
  )
import Graphics.Vty.Input.Events

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

handleProblemList :: TuiState -> Event -> EventM ResourceName TuiState
handleProblemList s e = do
  let currentFocus = tuiStateCurrentFocus s
  let oldProblemList = tuiStateProblemList s
  newProblemList <-
    if currentFocus == ProblemFocus
      then handleListEventVi (\_ l -> return l) e oldProblemList
      else return oldProblemList
  return s {tuiStateProblemList = newProblemList}

handleProblemDetail :: TuiState -> Event -> EventM ResourceName TuiState
handleProblemDetail s e = do
  let currentFocus = tuiStateCurrentFocus s
  let oldProblemDetail = tuiStateProblemDetail s
  case (oldProblemDetail, currentFocus) of
    (Just oldProblemDetail, DetailFocus) -> do
      let oldCodeDefinitionList = codeDefinitionList oldProblemDetail
      newCodeDefinitionList <- handleListEventVi (\_ l -> return l) e oldCodeDefinitionList
      let newProblemDetail = ProblemDetailList {slug = slug oldProblemDetail, content = content oldProblemDetail, codeDefinitionList = newCodeDefinitionList}
      return $ s {tuiStateProblemDetail = Just newProblemDetail}
    _ -> return s

handleOtherEvent :: TuiState -> Event -> NewState
handleOtherEvent s (EvKey (KChar 'q') []) = halt s
handleOtherEvent s (EvKey (KChar 'l') []) = do
  newState <- handleFocusDetail s
  continue newState
handleOtherEvent s (EvKey (KChar 'h') []) = do
  newState <- handleFocusProblem s
  continue newState
handleOtherEvent s (EvKey KEnter []) =
  if tuiStateCurrentFocus s == DetailFocus
    then do
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

      continue s
    else continue s
handleOtherEvent s _ = continue s

handleTuiEvent :: TuiState -> BrickEvent n e -> NewState
handleTuiEvent s e =
  case e of
    VtyEvent vtye -> do
      s1 <- handleProblemList s vtye
      s2 <- handleProblemDetail s1 vtye
      handleOtherEvent s2 vtye
    _ -> continue s
