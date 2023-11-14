module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TextToolbox

main = defaultMain $ testGroup "4. Übungsblatt" [exercise41, exercise42]

exercise41 = testGroup "Textstatistik" $ [
  testCase "countChars" $
     countChars "The quick brown fox jumps over the lazy dog."  @?= 44,
  testCase "countCharsWOSpaces" $
     countCharsWOSpaces "The quick brown fox jumps over the lazy dog."  @?= 36,
  testCase "countWords" $
     countWords "The quick brown fox jumps over the lazy dog."  @?= 9,
  testCase "averageWordLength" $
     averageWordLength "The quick brown fox jumps over the lazy dog." @?= 4.0,
  testCase "freqOfChars" $
     freqOfChars "The quick brown fox jumps over the lazy dog." @?= 
     [('O',4),('E',3),('H',2),('R',2),('T',2),('U',2),('A',1),('B',1),('C',1),('D',1),('F',1),('G',1),('I',1),('J',1),('K',1),('L',1),('M',1),('N',1),('P',1),('Q',1),('S',1),('V',1),('W',1),('X',1),('Y',1),('Z',1)]
  ]

exercise42 = testGroup "Texte mit Format" $ [
  testCase "formatText" $
     formatText "The quick brown fox jumps over the lazy dog." 15 @?= 
     ["The quick brown","fox jumps over","the lazy dog."],
  testCase "formatText, line too short" $
     formatText "The quick BigBrownFox jumps over the lazy dog." 5 @?= 
     ["The quick","BigBrownFox","jumps over","the lazy","dog."],
  testCase "addLineNrs" $
     addLineNrs ["The quick brown","fox jumps over","the lazy dog."] @?= 
     ["001 The quick brown","002 fox jumps over","003 the lazy dog."],
  testCase "searchString" $
     searchString "fox" ["001 The quick brown","002 fox jumps over","003 the lazy dog."] @?= 
     ["002 fox jumps over"],
  testCase "nicelyPrint" $
     nicelyPrint ["001 The quick brown","002 fox jumps over","003 the lazy dog."] @?= 
     "001 The quick brown\n002 fox jumps over\n003 the lazy dog.\n"
  ]