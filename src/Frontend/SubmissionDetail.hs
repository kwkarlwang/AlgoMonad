{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.SubmissionDetail where

import Brick (Widget)
import Brick.Widgets.List as BL hiding (reverse)
import qualified Data.Text as T
import Frontend.State (ResourceName)
import Frontend.Utils (drawStr)

renderSubmission :: Bool -> BL.List ResourceName FilePath -> Widget ResourceName
renderSubmission = BL.renderList renderFunc
  where
    renderFunc = renderLang

renderLang :: Bool -> FilePath -> Widget ResourceName
renderLang bool path = widget
  where
    lang = T.unpack . head . T.splitOn "." . last . T.splitOn "/" . T.pack $ path
    widget = drawStr bool lang
