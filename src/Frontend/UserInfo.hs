{-# LANGUAGE OverloadedStrings #-}

module Frontend.UserInfo where

import Backend.UserInfo (UserInfo)
import Brick (Widget (Widget), hBox, str)
import Brick.Widgets.Center (hCenter)
import Frontend.State (ResourceName)

renderUserInfo :: UserInfo -> Widget ResourceName
renderUserInfo userInfo = hBox components
  where
    components = map (\(key, value) -> hCenter $ str $ key ++ ": " ++ value) userInfo
