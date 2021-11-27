{-# LANGUAGE OverloadedStrings #-}

module Frontend.App where

import Backend.Problem (Problem (difficulty, pid, title), getProblems)
import Backend.UserInfo (UserInfo, getUserInfo, requestUserInfo)
import Brick (App (..), BrickEvent (VtyEvent), EventM, Next, Widget, attrMap, bg, continue, defaultMain, halt, showFirstCursor, str, vBox, withAttr)
import Brick.Widgets.List as BL
import Data.Vector as V
import Frontend.Problem as P
import Frontend.State (NewState, ResourceName (ProblemView), TuiState (TuiState, tuiStateProblemList, tuiStateProblems, tuiStateUserInfo))
import Frontend.Utils (makeCursor)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

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
      appAttrMap = const $ attrMap mempty [("current", withStyle (withForeColor (bg brightMagenta) black) bold)]
    }

buildInitialState :: IO TuiState
buildInitialState =
  do
    userInfo <- getUserInfo
    problems <- getProblems
    return $
      TuiState
        { tuiStateUserInfo = userInfo,
          tuiStateProblems = problems,
          tuiStateProblemList = BL.list ProblemView problems 1
        }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = Prelude.map (\f -> f ts) [P.renderProblem . tuiStateProblemList]

handleTuiEvent :: TuiState -> BrickEvent n e -> NewState
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey KDown [] -> moveDown
        EvKey (KChar 'j') [] -> moveDown
        EvKey KUp [] -> moveUp
        EvKey (KChar 'k') [] -> moveUp
        _ -> continue s
      where
        moveDown = continue $ s {tuiStateProblemList = BL.listMoveDown (tuiStateProblemList s)}
        moveUp = continue $ s {tuiStateProblemList = BL.listMoveUp (tuiStateProblemList s)}
    _ -> continue s
