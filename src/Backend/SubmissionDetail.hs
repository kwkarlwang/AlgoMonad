{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.SubmissionDetail where

import Backend.Utils
import Control.Concurrent (threadDelay)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.UTF8 as BSU
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Req

data SubmissionDetail = SubmissionDetail
  { submissionLang :: String,
    slug :: String,
    content :: String,
    pid :: Integer
  }
  deriving (Show)

data SubmissionReport
  = Accepted
      { lang :: String,
        totalCorrect :: Integer,
        totalTestCases :: Integer,
        statusMessage :: String,
        statusRuntime :: String,
        statusMemory :: String,
        runtimePercentile :: Float,
        memoryPercentile :: Float,
        stdOutput :: Maybe String
      }
  | WrongAnswer
      { lang :: String,
        totalCorrect :: Integer,
        totalTestCases :: Integer,
        lastTestCase :: String,
        statusMessage :: String,
        actualOutput :: String,
        expectedOutput :: String,
        stdOutput :: Maybe String
      }
  | RuntimeError
      { lang :: String,
        totalCorrect :: Integer,
        totalTestCases :: Integer,
        lastTestCase :: String,
        statusMessage :: String,
        actualOutput :: String,
        expectedOutput :: String,
        runtimeError :: String,
        stdOutput :: Maybe String
      }
  | CompileError
      { lang :: String,
        compileError :: String,
        statusMessage :: String
      }
  | LimitExceed
      { lang :: String,
        totalCorrect :: Integer,
        totalTestCases :: Integer,
        lastTestCase :: String,
        statusMessage :: String,
        stdOutput :: Maybe String
      }
  | Unknown
      { lang :: String,
        statusMessage :: String
      }
  deriving (Show)

requestSubmission :: (FromJSON a) => SubmissionDetail -> IO (Req (JsonResponse a))
requestSubmission problem = req POST url (ReqBodyJson payload) jsonResponse <$> headers
  where
    url = https "leetcode.com" /: "problems" /: T.pack (slug problem) /: "submit"
    referer = header "Referer" $ "https://leetcode.com/problems/" <> BSU.fromString (slug problem) <> "/"
    headers = (referer <>) <$> getCredentials
    payload =
      object
        [ "judge_type" .= ("large" :: String),
          "lang" .= submissionLang problem,
          "typed_code" .= content problem,
          "test_mode" .= False,
          "question_id" .= pid problem
        ]

requestVerification :: (FromJSON a) => Integer -> IO (Req (JsonResponse a))
requestVerification submissionId = req GET url NoReqBody jsonResponse <$> getCredentials
  where
    url = https "leetcode.com" /: "submissions" /: "detail" /: T.pack (show submissionId) /: "check"

readProblemFromFile :: FilePath -> String -> Integer -> IO SubmissionDetail
readProblemFromFile path slug pid = do
  content <- readFile path
  let submissionLang = T.unpack . head . T.splitOn "." . last . T.splitOn "/" . T.pack $ path
  return SubmissionDetail {..}

-- extractProblemReport

getVerfication :: Integer -> Integer -> IO Value
getVerfication submissionId count = do
  reqVerification <- requestVerification submissionId
  r <- runReq defaultHttpConfig reqVerification
  let reqData = responseBody r :: Value
  let state = reqData ^? key "state"
  case (state, count <= 60) of
    (Just (String "SUCCESS"), _) -> return reqData
    (_, True) -> do
      -- sleep for 0.5 seconds
      threadDelay 500000
      getVerfication submissionId (count + 1)
    _ -> error "Verify submission timeout. Please try submitting again."

extractAccepted :: Value -> SubmissionReport
extractAccepted value = output
  where
    lang = unpackString $ value ^? key "lang"
    totalCorrect = unpackInteger $ value ^? key "total_correct"
    totalTestCases = unpackInteger $ value ^? key "total_testcases"
    statusMessage = unpackString $ value ^? key "status_msg"
    statusRuntime = unpackString $ value ^? key "status_runtime"
    statusMemory = unpackString $ value ^? key "status_memory"
    runtimePercentile = unpackFloat $ value ^? key "runtimePercentile"
    memoryPercentile = unpackFloat $ value ^? key "memoryPercentile"
    stdOutput = case unpackString $ value ^? key "std_ouput" of
      "" -> Nothing
      x -> Just x
    output = Accepted {..}

extractWrongAnswer :: Value -> SubmissionReport
extractWrongAnswer value = output
  where
    lang = unpackString $ value ^? key "lang"
    totalCorrect = unpackInteger $ value ^? key "total_correct"
    totalTestCases = unpackInteger $ value ^? key "total_testcases"
    lastTestCase = unpackString $ value ^? key "last_testcase"
    statusMessage = unpackString $ value ^? key "status_msg"
    actualOutput = unpackString $ value ^? key "code_output"
    expectedOutput = unpackString $ value ^? key "expected_output"
    stdOutput = case unpackString $ value ^? key "std_ouput" of
      "" -> Nothing
      x -> Just x
    output = WrongAnswer {..}

extractRuntimeError :: Value -> SubmissionReport
extractRuntimeError value = output
  where
    lang = unpackString $ value ^? key "lang"
    totalCorrect = unpackInteger $ value ^? key "total_correct"
    totalTestCases = unpackInteger $ value ^? key "total_testcases"
    lastTestCase = unpackString $ value ^? key "last_testcase"
    statusMessage = unpackString $ value ^? key "status_msg"
    actualOutput = unpackString $ value ^? key "code_output"
    expectedOutput = unpackString $ value ^? key "expected_output"
    runtimeError = unpackString $ value ^? key "full_runtime_error"
    stdOutput = case unpackString $ value ^? key "std_ouput" of
      "" -> Nothing
      x -> Just x
    output = RuntimeError {..}

extractCompileError :: Value -> SubmissionReport
extractCompileError value = output
  where
    lang = unpackString $ value ^? key "lang"
    statusMessage = unpackString $ value ^? key "status_msg"
    compileError = unpackString $ value ^? key "full_compile_error"
    output = CompileError {..}

extractLimitExceed :: Value -> SubmissionReport
extractLimitExceed value = output
  where
    lang = unpackString $ value ^? key "lang"
    totalCorrect = unpackInteger $ value ^? key "total_correct"
    totalTestCases = unpackInteger $ value ^? key "total_testcases"
    lastTestCase = unpackString $ value ^? key "last_testcase"
    statusMessage = unpackString $ value ^? key "status_msg"
    stdOutput = case unpackString $ value ^? key "std_ouput" of
      "" -> Nothing
      x -> Just x
    output = LimitExceed {..}

extractUnknown value = output
  where
    lang = unpackString $ value ^? key "lang"
    statusMessage = unpackString $ value ^? key "status_msg"
    output = Unknown {..}

extractReport :: Value -> SubmissionReport
extractReport value = statusCodeToReport statusCode value
  where
    statusCode = case value ^? key "status_code" of
      Just (Integer x) -> x
      _ -> 21
    statusCodeToReport :: Integer -> Value -> SubmissionReport
    statusCodeToReport 10 = extractAccepted
    statusCodeToReport 11 = extractWrongAnswer
    statusCodeToReport 12 = extractLimitExceed
    statusCodeToReport 13 = extractLimitExceed
    statusCodeToReport 14 = extractLimitExceed
    statusCodeToReport 15 = extractRuntimeError
    statusCodeToReport 20 = extractCompileError
    statusCodeToReport _ = extractUnknown

getSubmissionReports :: FilePath -> String -> Integer -> IO SubmissionReport
getSubmissionReports path slug pid = do
  submitProblem <- readProblemFromFile path slug pid
  reqSubmission <- requestSubmission submitProblem
  r <- runReq defaultHttpConfig reqSubmission
  let reqData = responseBody r :: Value
  let submissionId = reqData ^? key "submission_id"
  case submissionId of
    Just (Integer submissionId) -> do
      reqData <- getVerfication submissionId 0
      return $ extractReport reqData
    _ -> error "cannot acquire submission"
