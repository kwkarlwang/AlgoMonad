{-# LANGUAGE OverloadedStrings #-}

module Frontend.App where

import qualified Backend.Problem as P
import qualified Backend.ProblemDetail as PD
import Backend.UserInfo (UserInfo, getUserInfo, requestUserInfo)
import Brick
  ( App (..),
    BrickEvent (VtyEvent),
    EventM,
    Next,
    Widget,
    attrMap,
    bg,
    continue,
    defaultMain,
    fg,
    hBox,
    hLimit,
    halt,
    showFirstCursor,
    str,
    vBox,
    withAttr,
  )
import Brick.Util (on)
import Brick.Widgets.Border (border, vBorder)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as BL
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (isNothing)
import qualified Data.Vector as V
import Frontend.KeyBinding (handleTuiEvent)
import qualified Frontend.Problem as P
import qualified Frontend.ProblemDetail as PD
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
import qualified Frontend.UserInfo as UI
import Graphics.Vty.Attributes

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
      appAttrMap =
        const $
          attrMap
            mempty
            [ ("white", fg brightWhite),
              ("selected" <> "black", withStyle (black `on` magenta) bold),
              ("red", fg red),
              ("selected" <> "red", withStyle (red `on` magenta) bold),
              ("yellow", fg yellow),
              ("selected" <> "yellow", withStyle (yellow `on` magenta) bold),
              ("green", fg green),
              ("selected" <> "green", withStyle (green `on` magenta) bold)
            ]
    }

buildInitialState :: IO TuiState
buildInitialState =
  do
    userInfo <- getUserInfo
    problems <- P.getProblems
    return $
      TuiState
        { tuiStateUserInfo = userInfo,
          tuiStateProblems = problems,
          tuiStateProblemList = BL.list ProblemView problems 1,
          tuiStateCurrentFocus = ProblemFocus,
          tuiStateProblemDetail = Nothing,
          tuiStateSearch = E.editor SearchView (Just 1) ""
        }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  [ vBox [userInfoWidget, problemWidget, searchWidget]
  ]
  where
    currentFocus = tuiStateCurrentFocus ts
    userInfoWidget = UI.renderUserInfo $ tuiStateUserInfo ts
    problemListWidget = P.renderProblem (currentFocus == ProblemFocus) $ tuiStateProblemList ts
    problemDetailWidget = case tuiStateProblemDetail ts of
      Nothing -> undefined
      Just problemDetail -> PD.renderProblemDetail (currentFocus == DetailFocus) problemDetail

    problemWidget =
      if isNothing (tuiStateProblemDetail ts)
        then problemListWidget
        else hBox [problemListWidget, problemDetailWidget]
    searchWidget = E.renderEditor (str . unlines) (currentFocus == SearchFocus) (tuiStateSearch ts)
