{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import Data.Aeson
import Frontend.Utils (floatDiv, floatRound)
import Test.HUnit

test1 = TestCase (assertEqual "Float division of two intergers" (floatDiv 3 2) 1.5)

test2 = TestCase (assertEqual "Round down to two decimal" (floatRound 1.23456 2) 1.23)

test3 = TestCase (assertEqual "Round up to two decimal" (floatRound 1.28956 2) 1.29)

test4 = TestCase (assertEqual "Round to five decimal" (floatRound 123123.28956 5) 123123.28956)

tests = [test1, test2, test3, test4]
