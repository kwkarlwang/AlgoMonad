module Utils.Filter where

import Data.List (intercalate, isInfixOf)
import Data.List.Split (splitOn)

openIdentifier :: String
openIdentifier = ">>>"

closeIdentifier :: String
closeIdentifier = "<<<"

filterByIdentifier :: ([String], Bool) -> String -> ([String], Bool)
filterByIdentifier (acc, True) line = (acc, shouldFilter)
  where
    shouldFilter = not $ closeIdentifier `isInfixOf` line
filterByIdentifier (acc, False) line = (newAcc, shouldFilter)
  where
    shouldFilter = openIdentifier `isInfixOf` line
    newAcc = if shouldFilter then acc else acc ++ [line]

filterContent :: String -> String
filterContent content = "\n" `intercalate` filteredSplits
  where
    splitByNewlines = splitOn "\n" content
    filteredSplits = fst $ foldl filterByIdentifier ([], False) splitByNewlines
