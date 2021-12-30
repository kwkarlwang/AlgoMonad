{-# LANGUAGE OverloadedStrings #-}

module Frontend.SubmissionDetail where

import Backend.Submission (Submission (slug), pid)
import Brick (Widget, padLeftRight)
import Brick.Widgets.List as BL hiding (reverse)
import qualified Data.Text as T
import Frontend.State (ResourceName)
import Frontend.Utils (drawStr)

renderSubmission :: Bool -> BL.List ResourceName FilePath -> Widget ResourceName
renderSubmission = BL.renderList renderFunc
  where
    renderFunc = renderLang

renderLang :: Bool -> FilePath -> Widget ResourceName
renderLang bool path = padLeftRight 1 widget
  where
    lang = T.unpack . head . T.splitOn "." . last . T.splitOn "/" . T.pack $ path
    widget = drawStr bool lang
