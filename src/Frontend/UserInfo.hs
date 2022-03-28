{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.UserInfo where

import Backend.UserInfo (UserInfo)
import Brick (Widget, hBox)
import Brick.Widgets.Center (hCenter)
import Frontend.State (ResourceName)
import Frontend.Utils (drawGreen, drawStr)

renderUserInfo :: UserInfo -> Widget ResourceName
renderUserInfo userInfo = hBox components
  where
    components = map (\(key, value) -> hCenter $ hBox [drawStr False (key ++ ": "), drawGreen False value]) userInfo
