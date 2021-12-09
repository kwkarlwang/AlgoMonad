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
    Padding (Max),
    Widget,
    attrMap,
    bg,
    continue,
    defaultMain,
    emptyWidget,
    fg,
    hBox,
    hLimit,
    hLimitPercent,
    halt,
    padBottom,
    showFirstCursor,
    str,
    vBox,
    vLimitPercent,
    withAttr,
    (<=>),
  )
import Brick.Util (on)
import Brick.Widgets.Border (border, borderAttr, vBorder)
import Brick.Widgets.Center (center)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as BL
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (isNothing)
import qualified Data.Vector as V
import Frontend.Help (renderHelp)
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
        tuiStateMessage,
        tuiStateProblemDetail,
        tuiStateProblemList,
        tuiStateSearch,
        tuiStateUserInfo
      ),
  )
import qualified Frontend.UserInfo as UI
import Frontend.Utils (drawGreen, drawStr)
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
              ("selected" <> "red", withStyle (black `on` magenta) bold),
              ("yellow", fg yellow),
              ("selected" <> "yellow", withStyle (black `on` magenta) bold),
              ("green", fg green),
              ("selected" <> "green", withStyle (black `on` magenta) bold),
              (borderAttr, fg brightWhite)
            ]
    }

buildInitialState :: IO TuiState
buildInitialState =
  do
    userInfo <- getUserInfo
    problems <- P.getProblems
    let isCurrentUserPremium = snd $ head $ filter (\tup -> fst tup == "premium") userInfo
    let filterProblems = case isCurrentUserPremium of
          "False" -> V.filter (not . P.paidOnly) problems
          _ -> problems
    return $
      TuiState
        { tuiStateUserInfo = userInfo,
          tuiStateProblemList = BL.list ProblemView problems 1,
          tuiStateCurrentFocus = ProblemFocus,
          tuiStateProblemDetail = Nothing,
          tuiStateSearch = E.editor SearchView (Just 1) "",
          tuiStateMessage = Nothing
        }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  [ vBox [userInfoWidget, problemWidget, bottomWidget]
  ]
  where
    currentFocus = tuiStateCurrentFocus ts
    userInfoWidget = UI.renderUserInfo $ tuiStateUserInfo ts
    problemListWidget = hLimitPercent 70 $ P.renderProblem (currentFocus == ProblemFocus) $ tuiStateProblemList ts
    problemDetailWidget = padBottom Max $ case tuiStateProblemDetail ts of
      Nothing -> str " "
      Just problemDetail -> PD.renderProblemDetail (currentFocus == DetailFocus) problemDetail

    rightSide = problemDetailWidget <=> renderHelp

    problemWidget = hBox [problemListWidget, vBorder, rightSide]
    bottomWidget = case tuiStateMessage ts of
      Nothing -> E.renderEditor (str . unlines) (currentFocus == SearchFocus) (tuiStateSearch ts)
      Just message -> drawGreen False message
