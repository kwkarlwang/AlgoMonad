{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module UserInfo.Render where

import Brick (Widget, hBox)
import Brick.Widgets.Center (hCenter)
import Frontend.State (ResourceName)
import Frontend.Utils (drawBoldCyan, drawBoldGreen, drawBoldRed, drawBoldStr, drawBoldYellow)
import UserInfo.State (QuestionCount (count, difficulty, solved), UserInfo (allQuestionCount, easyQuestionCount, hardQuestionCount, mediumQuestionCount, premium, username))

solvedOverCount :: QuestionCount -> String
solvedOverCount questionCount = res
  where
    totalCount = count questionCount
    totalSolved = solved questionCount
    res = show totalSolved ++ "/" ++ show totalCount

renderUserInfo :: UserInfo -> Widget ResourceName
renderUserInfo userInfo = hBox components
  where
    usernameWidget = hBox [drawBoldCyan "Username: ", drawBoldStr $ username userInfo]
    premiumWidget = hBox [drawBoldCyan "Premium: ", drawBoldStr $ show $ premium userInfo]

    allQuestion = allQuestionCount userInfo
    allCountWidget = hBox [drawBoldCyan $ difficulty allQuestion ++ ": ", drawBoldStr $ solvedOverCount allQuestion]

    easyQuestion = easyQuestionCount userInfo
    easyCountWidget = hBox [drawBoldGreen $ difficulty easyQuestion ++ ": ", drawBoldStr $ solvedOverCount easyQuestion]

    mediumQuestion = mediumQuestionCount userInfo
    mediumCountWidget = hBox [drawBoldYellow $ difficulty mediumQuestion ++ ": ", drawBoldStr $ solvedOverCount mediumQuestion]

    hardQuestion = hardQuestionCount userInfo
    hardCountWidget = hBox [drawBoldRed $ difficulty hardQuestion ++ ": ", drawBoldStr $ solvedOverCount hardQuestion]
    components = map hCenter [usernameWidget, premiumWidget, allCountWidget, easyCountWidget, mediumCountWidget, hardCountWidget]
