{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Main where 

import System.Exit

import Test.Tasty
import Common
import Test.QuickCheck
import UI.Resources

main :: IO ()
main = runTests 
  [ manualTests
  , autoTests
  ]

manualTests ::  Score -> TestTree
manualTests sc = testGroup "Manual Tests" 
  [ scoreTest ((\_ -> 1+1),  (), 2, 0, "test-add"), 
    scoreTest ((\x -> split x '\n'),  "a\nb\nc\n", ["a","b","c"], 0, "test-split-helper-1"),
    scoreTest ((\x -> split x '$'),  "a$b$c$", ["a","b","c"], 0, "test-split-helper-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

autoTests :: Score -> TestTree
autoTests sc = testGroup "Automated Tests"
  [ scoreProp sc ("prop_test", prop_test, 0) 
  ]

genRandN :: Gen Int
genRandN = elements [1..10]

prop_test = forAll genRandN (<5)