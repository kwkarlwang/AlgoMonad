{-# LANGUAGE OverloadedStrings #-}

module Screen where

import Brick
import Brick.Widgets.Center (center)
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Text
import Request (getResponseBody, getUserInfo, maybeToIO)
import Prelude hiding (head)

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
