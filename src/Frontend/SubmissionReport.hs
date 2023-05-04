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
    runtimeErrorWidget = renderError runtimeError
    testcasesWidget = renderTestcases totalCorrect totalTestcases
    lastTestcaseWidget = renderLastTestcaseError lastTestcase actualOutput expectedOutput stdOutput
    widget = vBox [slugWidget, statusWidget, runtimeErrorWidget, testcasesWidget, lastTestcaseWidget]
renderSubmissionReport CompileError {..} = widget
  where
    slugWidget = renderSlug slug
    statusWidget = renderStatus False statusMessage
    compileErrorWidget = renderError compileError
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
    widget =
      hBox
        [ drawStr "Testcases: pass ",
          correctWidget,
          drawStr " out of ",
          testcasesWidget,
          drawStr " testcases"
        ]

renderRuntime :: String -> Float -> String -> Widget ResourceName
renderRuntime statusRuntime runtimePercentile lang = widget
  where
    statusWidget = drawBoldStr statusRuntime
    percentileWidget = drawBoldStr $ show (floatRound runtimePercentile 2) ++ "%"
    langWidget = drawBoldStr lang
    widget =
      hBox
        [ drawStr "Runtime: ",
          statusWidget,
          drawStr ", faster than ",
          percentileWidget,
          drawStr " of ",
          langWidget,
          drawStr " online submissions"
        ]

renderMemory :: String -> Float -> String -> Widget ResourceName
renderMemory statusMemory memoryPercentile lang = widget
  where
    statusWidget = drawBoldStr statusMemory
    percentileWidget = drawBoldStr $ show (floatRound memoryPercentile 2) ++ "%"
    langWidget = drawBoldStr lang
    widget =
      hBox
        [ drawStr "Memory Usage: ",
          statusWidget,
          drawStr ", less than ",
          percentileWidget,
          drawStr " of ",
          langWidget,
          drawStr " online submissions"
        ]

renderLastTestcaseError :: String -> String -> String -> Maybe String -> Widget ResourceName
renderLastTestcaseError lastTestcase actualOutput expectedOutput stdOutput = widget
  where
    titleWidget = drawStr "Last Testcase:"
    inputWidget = drawStr "  Input:    " <+> drawBoldStr lastTestcase
    outputWidget = drawStr "  Output:   " <+> drawBoldRed actualOutput
    expectedWidget = drawStr "  Expected: " <+> drawBoldGreen expectedOutput
    stdOutputWidget = (drawStr "  Print:    " <+>) <$> (drawBoldStr <$> stdOutput)
    widget = case stdOutputWidget of
      (Just stdOutputWidget) -> vBox [titleWidget, inputWidget, outputWidget, expectedWidget, stdOutputWidget]
      Nothing -> vBox [titleWidget, inputWidget, outputWidget, expectedWidget]

renderLastTestcaseLimit :: String -> Maybe String -> Widget ResourceName
renderLastTestcaseLimit lastTestcase stdOutput = widget
  where
    titleWidget = drawStr "Last Testcase:"
    inputWidget = drawStr "  Input:    " <+> drawBoldStr lastTestcase
    stdOutputWidget = (drawStr "  Print:    " <+>) <$> (drawBoldStr <$> stdOutput)
    widget = case stdOutputWidget of
      (Just stdOutputWidget) -> vBox [titleWidget, inputWidget, stdOutputWidget]
      Nothing -> vBox [titleWidget, inputWidget]

renderError :: String -> Widget ResourceName
renderError errorMessage = widget
  where
    widget = drawRed errorMessage
