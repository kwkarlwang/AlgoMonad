{-# LANGUAGE OverloadedStrings #-}

module Frontend.App where

import Backend.Problem (Problem (difficulty, pid, title), getProblems)
import Backend.UserInfo (UserInfo, getUserInfo, requestUserInfo)
import Brick (App (..), BrickEvent (VtyEvent), EventM, Next, Widget, attrMap, bg, continue, defaultMain, halt, showFirstCursor, str, vBox, withAttr)
import Brick.Widgets.List as BL
import Cursor.Simple.List.NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Vector as V hiding (map)
import Frontend.Problem as P
import Frontend.ProblemList as PL
import Frontend.State (NewState, ResourceName (Viewport1), TuiState (TuiState, tuiRenderProblems, tuiStateProblems, tuiStateUserInfo))
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
          tuiStateProblems = BL.list Viewport1 problems 1,
          tuiRenderProblems = makeCursor $ V.toList problems
        }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = map (\f -> f ts) [PL.renderProblem . tuiStateProblems]

handleTuiEvent :: TuiState -> BrickEvent n e -> NewState
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        -- EvKey KDown [] -> moveDown
        -- EvKey (KChar 'j') [] -> moveDown
        -- EvKey KUp [] -> moveUp
        -- EvKey (KChar 'k') [] -> moveUp
        _ -> continue s
    -- where
    --   moveDown = select nonEmptyCursorSelectNext s
    --   moveUp = select nonEmptyCursorSelectPrev s
    _ -> continue s
