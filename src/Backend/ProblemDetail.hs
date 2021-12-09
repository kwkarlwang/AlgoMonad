{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.ProblemDetail where

import Backend.Utils
import qualified Brick.Widgets.List as BL
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Text (encodeToLazyText)
import Data.List (intercalate)
import qualified Data.Text as T (Text, unpack)
import qualified Data.Text.Lazy as TL (fromStrict)
import qualified Data.Text.Lazy.Encoding as TLE (encodeUtf8)
import qualified Data.Vector as V
import Network.HTTP.Req
import qualified System.Directory as DIR

data ProblemDetail = ProblemDetail
  { slug :: String,
    content :: String,
    codeDefinitionVector :: V.Vector (String, String)
  }
  deriving (Show)

langToExtension =
  [ ("c", ".c"),
    ("cpp", ".cpp"),
    ("csharp", ".cs"),
    ("golang", ".go"),
    ("java", ".java"),
    ("javascript", ".js"),
    ("typescript", ".ts"),
    ("kotlin", ".kt"),
    ("php", ".php"),
    ("python", ".py"),
    ("python3", ".py"),
    ("ruby", ".rb"),
    ("rust", ".rs"),
    ("scala", ".scala"),
    ("swift", ".swift"),
    ("racket", ".rkt"),
    ("erlang", ".erl"),
    ("elixir", ".exs")
  ]

requestProblemDetail :: (FromJSON a) => String -> IO (Req (JsonResponse a))
requestProblemDetail slug = do
  req POST (https "leetcode.com" /: "graphql") (ReqBodyJson payload) jsonResponse <$> getCredentials
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

valueToText :: Value -> T.Text
valueToText (String x) = x
valueToText _ = error "Not Text"

makeProblemDetail :: Array -> Maybe (V.Vector (String, String))
makeProblemDetail codeDefinition =
  do
    codeKey <- V.mapM (\value -> T.unpack . valueToText <$> value ^? key "value") codeDefinition
    codeValue <- V.mapM (\value -> T.unpack . valueToText <$> value ^? key "defaultCode") codeDefinition
    return $ V.zip codeKey codeValue

getProblemDetail :: String -> IO ProblemDetail
getProblemDetail slug = do
  request <- requestProblemDetail slug
  reqData <- getResponseBody request
  if reqData == Null
    then error "Need to re login in the browser"
    else case ( do
                  questionData <- reqData ^? key "question"
                  content <- T.unpack . valueToText <$> questionData ^? key "content"
                  codeDefinitionStr <- valueToText <$> questionData ^? key "codeDefinition"
                  let codeDefinitionText = TLE.encodeUtf8 $ TL.fromStrict codeDefinitionStr
                  codeDefinitionValue <- decode codeDefinitionText :: Maybe Array
                  codeDefinitionVector <- makeProblemDetail codeDefinitionValue
                  return $ ProblemDetail {..}
              ) of
      Just x -> return x
      Nothing -> error "error parsing"

writeProblemToFile :: String -> String -> (String, String) -> IO ()
writeProblemToFile slug content codeDefinitionPair = do
  let folderPath = "./" ++ slug ++ ".algomonad"
  let contentText = content
  let (codeLang, codeText) = codeDefinitionPair
  let codeFileExtension = snd $ head $ filter (\tup -> fst tup == codeLang) langToExtension
  DIR.createDirectoryIfMissing True folderPath
  writeFile (folderPath ++ "/" ++ slug ++ ".html") contentText
  writeFile (folderPath ++ "/" ++ slug ++ codeFileExtension) codeText
  return ()
