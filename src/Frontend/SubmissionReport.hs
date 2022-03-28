{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Frontend.SubmissionReport where

import Backend.SubmissionDetail
import Brick (Widget, hBox, vBox, (<+>))
import Frontend.State
import Frontend.Utils

renderSubmissionReport :: SubmissionReport -> Widget ResourceName
renderSubmissionReport Accepted {..} = widget
  where
    slugWidget = renderSlug slug
    statusWidget = renderStatus True statusMessage
    testcasesWidget = renderTestcases totalCorrect totalTestcases
    runtimeWidget = renderRuntime statusRuntime runtimePercentile lang
    memoryWidget = renderMemory statusMemory memoryPercentile lang
    widget = vBox [slugWidget, statusWidget, testcasesWidget, runtimeWidget, memoryWidget]
renderSubmissionReport WrongAnswer {..} = widget
  where
    slugWidget = renderSlug slug
    statusWidget = renderStatus False statusMessage
    testcasesWidget = renderTestcases totalCorrect totalTestcases
    lastTestcaseWidget = renderLastTestcaseError lastTestcase actualOutput expectedOutput stdOutput
    widget = vBox [slugWidget, statusWidget, testcasesWidget, lastTestcaseWidget]
renderSubmissionReport RuntimeError {..} = widget
  where
    slugWidget = renderSlug slug
    statusWidget = renderStatus False statusMessage
    testcasesWidget = renderTestcases totalCorrect totalTestcases
    lastTestcaseWidget = renderLastTestcaseError lastTestcase actualOutput expectedOutput stdOutput
    widget = vBox [slugWidget, statusWidget, testcasesWidget, lastTestcaseWidget]
renderSubmissionReport CompileError {..} = widget
  where
    slugWidget = renderSlug slug
    statusWidget = renderStatus False statusMessage
    compileErrorWidget = renderCompileError compileError
    widget = vBox [slugWidget, statusWidget, compileErrorWidget]
renderSubmissionReport LimitExceed {..} = widget
  where
    slugWidget = renderSlug slug
    statusWidget = renderStatus False statusMessage
    testcasesWidget = renderTestcases totalCorrect totalTestcases
    lastTestcaseWidget = renderLastTestcaseLimit lastTestcase stdOutput
    widget = vBox [slugWidget, statusWidget, testcasesWidget, lastTestcaseWidget]
renderSubmissionReport Unknown {..} = widget
  where
    slugWidget = renderSlug slug
    statusWidget = renderStatus False statusMessage
    widget = vBox [slugWidget, statusWidget]

renderSlug :: String -> Widget ResourceName
renderSlug = drawBoldCyan

renderStatus :: Bool -> String -> Widget ResourceName
renderStatus True = drawBoldGreen
renderStatus False = drawBoldRed

renderTestcases :: Integer -> Integer -> Widget ResourceName
renderTestcases totalCorrect totalTestcases = widget
  where
    correctWidget = drawBoldStr $ show totalCorrect
    testcasesWidget = drawBoldStr $ show totalTestcases
    f = drawStr False
    widget =
      hBox
        [ f "Testcases: pass ",
          correctWidget,
          f " out of ",
          testcasesWidget,
          f " testcases"
        ]

renderRuntime :: String -> Float -> String -> Widget ResourceName
renderRuntime statusRuntime runtimePercentile lang = widget
  where
    statusWidget = drawBoldStr statusRuntime
    percentileWidget = drawBoldStr $ show (floatRound runtimePercentile 2) ++ "%"
    langWidget = drawBoldStr lang
    f = drawStr False
    widget =
      hBox
        [ f "Runtime: ",
          statusWidget,
          f ", faster than ",
          percentileWidget,
          f " of ",
          langWidget,
          f " online submissions"
        ]

renderMemory :: String -> Float -> String -> Widget ResourceName
renderMemory statusMemory memoryPercentile lang = widget
  where
    statusWidget = drawBoldStr statusMemory
    percentileWidget = drawBoldStr $ show (floatRound memoryPercentile 2) ++ "%"
    langWidget = drawBoldStr lang
    f = drawStr False
    widget =
      hBox
        [ f "Memory Usage: ",
          statusWidget,
          f ", less than ",
          percentileWidget,
          f " of ",
          langWidget,
          f " online submissions"
        ]

renderLastTestcaseError :: String -> String -> String -> Maybe String -> Widget ResourceName
renderLastTestcaseError lastTestcase actualOutput expectedOutput stdOutput = widget
  where
    f = drawStr False
    titleWidget = f "Last Testcase:"
    inputWidget = f "  Input:    " <+> drawBoldStr lastTestcase
    outputWidget = f "  Output:   " <+> drawBoldRed actualOutput
    expectedWidget = f "  Expected: " <+> drawBoldGreen expectedOutput
    stdOutputWidget = (f "  Print:    " <+>) <$> (drawBoldStr <$> stdOutput)
    widget = case stdOutputWidget of
      (Just stdOutputWidget) -> vBox [titleWidget, inputWidget, outputWidget, expectedWidget, stdOutputWidget]
      Nothing -> vBox [titleWidget, inputWidget, outputWidget, expectedWidget]

renderLastTestcaseLimit :: String -> Maybe String -> Widget ResourceName
renderLastTestcaseLimit lastTestcase stdOutput = widget
  where
    f = drawStr False
    titleWidget = f "Last Testcase:"
    inputWidget = f "  Input:    " <+> drawBoldStr lastTestcase
    stdOutputWidget = (f "  Print:    " <+>) <$> (drawBoldStr <$> stdOutput)
    widget = case stdOutputWidget of
      (Just stdOutputWidget) -> vBox [titleWidget, inputWidget, stdOutputWidget]
      Nothing -> vBox [titleWidget, inputWidget]

renderCompileError :: String -> Widget ResourceName
renderCompileError compileError = widget
  where
    f = drawStr False
    widget = f compileError
