module Main where

import Test.Tasty
import Test.Tasty.HUnit

import ListUtils
import Goldbach

main = defaultMain $ testGroup "3. Übungsblatt" [exercise31, exercise32]

exercise31 = testGroup "ListUtils" $ [
  testCase "containsDoubleElem_True" $
     containsDoubleElem "Hallo!"  @?= True,
  testCase "containsDoubleElem_False" $
     containsDoubleElem [1,2,1]  @?= False,
  testCase "removeDoubleElem" $
     removeDoubleElem "Hallo! Huhu!"  @?= "Halo! Huhu!",
  testCase "isPalindrome_True" $
     isPalindrome "ABBA" @?= True,
  testCase "isPalindrome_False" $
     isPalindrome "PAPA" @?= False,
  testCase "computeTip" $
     computeTip [79.0, 19.2, 102.5] @?= [(79.0,87), (19.2,21), (102.5,113)]
  ]

exercise32 = testGroup "Goldbach" $ [
  testCase "nextPrime_1" $
     nextPrime 19 @?= 23,
  testCase "nextPrime_2" $
     nextPrime 1000 @?= 1009,
  testCase "primesUpTo_1" $
     primesUpTo 7 @?= [2,3,5,7],
  testCase "primesUpTo_2" $
     primesUpTo 50 @?= [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47],
  testCase "goldbach" $
     goldbach 30 @?= [(7,23), (11,19), (13,17)],
  testCase "goldbach2" $
     goldbach2 1002 @?= (1002, 5, 997)
  ]