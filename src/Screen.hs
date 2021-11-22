{-# LANGUAGE OverloadedStrings #-}

module Screen where

import Brick
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Cursor.Simple.List.NonEmpty
import Data.Aeson
import Data.Aeson.Lens
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Request.Problems (Problem (difficulty, pid, title), Problems, getProblems)
import Request.UserInfo (UserInfo, getUserInfo, requestUserInfo)

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  -- print endState
  return ()

data TuiState = TuiState {tuiStateUserInfo :: UserInfo, tuiStateProblems :: Problems, tuiRenderProblems :: NonEmptyCursor Problem}
  deriving (Show, Eq)

type ResourceName = String

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
          tuiRenderProblems = do
            case NE.nonEmpty problems of
              Nothing -> undefined
              Just ne -> makeNonEmptyCursor ne
        }

drawTui :: TuiState -> [Widget ResourceName]
-- drawTui ts = [vBox $ map (str . uncurry (++)) $ tuiStateUserInfo ts]
drawTui ts =
  let renderProblem = \problem -> show (pid problem) ++ " " ++ title problem
      nec = tuiRenderProblems ts
   in [ vBox $
          concat
            [ map (drawStr False . renderProblem) $ reverse $ nonEmptyCursorPrev nec,
              [drawStr True $ renderProblem $ nonEmptyCursorCurrent nec],
              map (drawStr False . renderProblem) $ nonEmptyCursorNext nec
            ]
      ]

drawStr :: Bool -> String -> Widget n
drawStr b = (if b then withAttr "current" else id) . str

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
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
        moveDown = do
          let nec = tuiRenderProblems s
          case nonEmptyCursorSelectNext nec of
            Nothing -> continue s
            Just nec' -> continue $ s {tuiRenderProblems = nec'}
        moveUp = do
          let nec = tuiRenderProblems s
          case nonEmptyCursorSelectPrev nec of
            Nothing -> continue s
            Just nec' -> continue $ s {tuiRenderProblems = nec'}
    _ -> continue s

-- problemsList :: Widget ()
-- problemsList = do
--   obj <- getResponseBody getUserInfo
--   obj
--   str "Center"

-- userInfo :: Widget String
-- userInfo = do
--   obj <- getResponseBody getUserInfo
--   case obj of
--     Bool False -> return "Error occured"
--     _ -> do
--       let username = (maybeToIO $ obj ^? key "user" . key "username") :: IO Value
--       return $ show $ liftIO username
