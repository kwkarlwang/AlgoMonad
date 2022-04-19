{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.Submission where

import Backend.Utils
import Control.Monad (filterM)
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sortBy)
import qualified System.Directory as DIR

data Submission = Submission
  { pid :: Integer,
    slug :: String,
    folderPath :: FilePath,
    langPaths :: V.Vector FilePath
  }
  deriving (Show)

extractPid :: FilePath -> Integer
extractPid = read . T.unpack . head . T.splitOn "." . last . T.splitOn "/" . T.pack

extractSlug :: FilePath -> String
extractSlug path = T.unpack $ (T.splitOn "." . last . T.splitOn "/" . T.pack) path !! 1

extractSubmission :: FilePath -> IO Submission
extractSubmission folderPath = do
  let names = map (uncurry (++)) langExtensionPair
  filterLangs <- filter (`elem` names) <$> DIR.listDirectory folderPath
  let langPaths = V.fromList $ map (\name -> folderPath ++ "/" ++ name) filterLangs
  let pid = extractPid folderPath
  let slug = extractSlug folderPath
  return Submission {..}

getSubmissions :: IO (V.Vector Submission)
getSubmissions = do
  currentDir <- DIR.getCurrentDirectory
  folders <- map (\path -> currentDir ++ "/" ++ path) <$> DIR.listDirectory currentDir
  filterFolders <- filterM (\path -> (L.isSuffixOf ".algomonad" path &&) <$> DIR.doesDirectoryExist path) folders
  submissionList <- mapM extractSubmission filterFolders
  return $ V.modify (sortBy (comparing pid)) $ V.fromList submissionList
