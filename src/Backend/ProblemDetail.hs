{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.ProblemDetail where

import Backend.Utils
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.List (intercalate)
import qualified Data.Text as T (Text, unpack)
import qualified Data.Vector as V
import Network.HTTP.Req
import qualified System.Directory as DIR

data ProblemDetail = ProblemDetail
  { pid :: Integer,
    slug :: String,
    content :: String,
    codeDefinitionVector :: V.Vector (String, String),
    likes :: Integer,
    dislikes :: Integer
  }
  deriving (Eq, Show)

requestProblemDetail :: (FromJSON a) => String -> IO (Req (JsonResponse a))
requestProblemDetail slug = req POST (https "leetcode.com" /: "graphql") (ReqBodyJson payload) jsonResponse <$> getCredentials
  where
    payload =
      object
        [ "query"
            .= intercalate
              "\n"
              [ "query getQuestionDetail($titleSlug: String!) {",
                "  question(titleSlug: $titleSlug) {",
                "    content",
                "    codeSnippets {",
                "       langSlug",
                "       code",
                "    }",
                "    likes",
                "    dislikes",
                "  }",
                "}"
              ],
          "variables" .= object ["titleSlug" .= slug],
          "operationName" .= ("getQuestionDetail" :: String)
        ]

valueToText :: Value -> T.Text
valueToText (String x) = x
valueToText _ = error "Not Text"

makeProblemDetailCode :: Array -> Maybe (V.Vector (String, String))
makeProblemDetailCode codeSnippets =
  do
    codeKey <- V.mapM (\value -> T.unpack <$> value ^? key "langSlug" . _String) codeSnippets
    codeValue <- V.mapM (\value -> T.unpack <$> value ^? key "code" . _String) codeSnippets
    return $ V.zip codeKey codeValue

extractProblemDetail :: String -> Value -> Integer -> Maybe ProblemDetail
extractProblemDetail slug value pid = do
  questionData <- value ^? key "question"
  content <- T.unpack <$> questionData ^? key "content" . _String
  codeSnippets <- questionData ^? key "codeSnippets" . _Array
  codeDefinitionVector <- makeProblemDetailCode codeSnippets
  likes <- questionData ^? key "likes" . _Integer
  dislikes <- questionData ^? key "dislikes" . _Integer
  return $ ProblemDetail {..}

getProblemDetail :: String -> Integer -> IO ProblemDetail
getProblemDetail slug pid = do
  request <- requestProblemDetail slug
  reqData <- getResponseBody request
  if reqData == Null
    then error "Need to re login in the browser"
    else case extractProblemDetail slug reqData pid of
      Just x -> return x
      Nothing -> error "error parsing problem detail"

writeProblemToFile :: String -> String -> (String, String) -> Integer -> IO ()
writeProblemToFile slug content codeDefinitionPair pid = do
  let folderPath = "./" ++ show pid ++ "." ++ slug ++ ".algomonad"
  let contentText = content
  let (codeLang, codeText) = codeDefinitionPair
  let codeFileExtension = langToExtension codeLang
  DIR.createDirectoryIfMissing True folderPath
  writeFile (folderPath ++ "/" ++ "writeup.html") $ darkModeCss ++ contentText
  writeFile (folderPath ++ "/" ++ codeLang ++ codeFileExtension) codeText
  return ()
