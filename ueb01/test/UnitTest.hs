module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TextFrame
import PrimeTwins

main = defaultMain $ testGroup "1. Übungsblatt" [exercise11, exercise12]

exercise11 = testGroup "TextFrame" $ [
  testCase "Hello Haskell" $
     textInFrame "Hello Haskell!" '*'  @?= "******************\n* Hello Haskell! *\n******************\n"
  ]

exercise12 = testGroup "PrimeTwins" $ [
  testCase "divides: False" $
     divides 3 7 @?= False,
  testCase "divides: True" $
     divides 7 35 @?= True,
  testCase "isPrime: True" $
     isPrime 7 @?= True,
  testCase "isPrime: False" $
     isPrime 21 @?= False,
  testCase "isPrimeTwin: True" $
     isPrimeTwin (17,19) @?= True,
  testCase "isPrimeTwin: False" $
     isPrimeTwin (19,23) @?= False
  ]