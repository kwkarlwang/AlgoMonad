{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.ProblemDetail where

import Backend.Utils
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Text (encodeToLazyText)
import Data.List (intercalate)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Debug.Trace (traceM)
import GHC.Exts
import Network.HTTP.Req

data ProblemDetail = ProblemDetail
  { slug :: String,
    content :: String,
    codeDefinition :: [(String, String)]
  }
  deriving (Eq, Show)

requestProblemDetail :: (FromJSON a) => String -> Req (JsonResponse a)
requestProblemDetail slug =
  req POST (https "leetcode.com" /: "graphql") (ReqBodyJson payload) jsonResponse headers
  where
    payload =
      object
        [ "query"
            .= intercalate
              "\n"
              [ "query getQuestionDetail($titleSlug: String!) {",
                "  question(titleSlug: $titleSlug) {",
                "    content",
                "    codeDefinition",
                "  }",
                "}"
              ],
          "variables" .= object ["titleSlug" .= slug],
          "operationName" .= ("getQuestionDetail" :: String)
        ]
    headers = getCredentials

valueToText :: Value -> T.Text
valueToText (String x) = x
valueToText _ = error "Not Text"

makeProblemDetail :: [Value] -> Maybe [(String, String)]
makeProblemDetail codeDefinition =
  do
    codeKey <- mapM (\value -> T.unpack . valueToText <$> value ^? key "value") codeDefinition
    codeValue <- mapM (\value -> T.unpack . valueToText <$> value ^? key "defaultCode") codeDefinition
    return $ zip codeKey codeValue

getProblemDetail :: String -> IO ProblemDetail
getProblemDetail slug = do
  reqData <- getResponseBody $ requestProblemDetail slug
  if reqData == Null
    then error "Need to re login in the browser"
    else case ( do
                  questionData <- reqData ^? key "question"
                  content <- T.unpack . valueToText <$> questionData ^? key "content"
                  codeDefinitionStr <- valueToText <$> questionData ^? key "codeDefinition"
                  let codeDefinitionText = TLE.encodeUtf8 $ TL.fromStrict codeDefinitionStr
                  codeDefinitionValue <- (decode codeDefinitionText :: Maybe [Value])
                  codeDefinition <- makeProblemDetail codeDefinitionValue
                  return $ ProblemDetail {..}
              ) of
      Just x -> return x
      Nothing -> error "error parsing"
