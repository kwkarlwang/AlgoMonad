{-# LANGUAGE OverloadedStrings #-}

module Screen where

import Brick
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import Graphics.Vty.Input.Events
import Request (getResponseBody, getUserInfo, maybeToIO)

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

data TuiState = TuiState {tuiStateUserInfo :: [(String, String)]}
  deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = return,
      appAttrMap = const $ attrMap mempty []
    }

buildInitialState :: IO TuiState
buildInitialState =
  do
    obj <- getResponseBody getUserInfo
    return $
      TuiState
        { tuiStateUserInfo = case obj of
            Bool False -> [("Error occured", "")]
            _ ->
              let username = obj ^? key "user" . key "username"
               in case username of
                    Just (String x) -> [("username: ", init . tail . show $ x)]
                    _ -> [("Error occured", "")]
        }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = map (str . uncurry (++)) $ tuiStateUserInfo ts

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
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
