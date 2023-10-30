module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Enigma

main = defaultMain $ testGroup "2. Übungsblatt" [exercise21, exercise22]

exercise21 = testGroup "Encode/Decode" $ [
  testCase "encodeChar" $
     encodeChar 'C' rotor1  @?= 'P',
  testCase "turnRotor" $
     turnRotor rotor1  @?= "KPQRLYZADGVWXHMNOIEFBSTUCJ",
  testCase "turnRotorByN" $
     turnRotorByN rotor1 5 @?= "LYZADGVWXHMNOIEFBSTUCJKPQR",
  testCase "enigma" $
     enigma "HASKELL" rotor1 6 8 @?= "XHHHNLX",
  testCase "indexOfChar" $
     indexOfChar 'B' rotor1 @?= 21,
  testCase "decodeChar" $
     decodeChar 'C' rotor1 @?= 'Z',
  testCase "deEnigma" $
     deEnigma "XHHHNLX" rotor1 6 8 @?= "HASKELL"
  ]

exercise22 = testGroup "Code Breaking" $ [
  testCase "breakEnigma" $
     take 50 (breakEnigma "NCP" rotor1) @?= "QZC\nQYA\nQXY\nQWW\nQVU\nQUS\nQTQ\nQSO\nQRM\nQQK\nQPI\nQOG\nQN",
  testCase "breakEnigmaWithGuess" $
     breakEnigmaWithGuess "NGGZAPCCWUUFMNB" rotor1 "UND" @?= "HUNDXKATZEXMAUS\n"
  ]