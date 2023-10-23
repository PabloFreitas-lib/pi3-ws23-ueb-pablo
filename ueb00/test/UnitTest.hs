module Main where

import Test.Tasty
import Test.Tasty.HUnit

import SumISq

main = defaultMain $ testGroup "0. Übungsblatt" [exercise0]

exercise0 = testGroup "SumISq" $ [
  testCase "sumISq 5" $
     sumISq 5 @?= 55,
  testCase "sumISq 0" $ 
     sumISq 0 @?= 0
  ]
