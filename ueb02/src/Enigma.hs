module Enigma where

import Data.Char
import Data.List

type Rotor = String

rotor0 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
rotor1 = "JKPQRLYZADGVWXHMNOIEFBSTUC"


encodeChar :: Char -> Rotor -> Char
encodeChar = undefined

turnRotor :: Rotor -> Rotor
turnRotor = undefined

turnRotorByN :: Rotor -> Int -> Rotor
turnRotorByN = undefined

enigma :: String -> Rotor -> Int -> Int -> String
enigma = undefined

indexOfChar :: Char -> Rotor -> Int
indexOfChar = undefined

decodeChar :: Char -> Rotor -> Char
decodeChar = undefined

deEnigma :: String -> Rotor -> Int -> Int -> String
deEnigma = undefined


breakEnigma :: String -> Rotor -> String
breakEnigma = undefined

breakEnigmaWithGuess :: String -> Rotor -> String -> String
breakEnigmaWithGuess = undefined


secretMessage = "EQWJTEMXUPICNOLPUNUDJDOWIZJANGKXDJFONQNRQQYGFLMGRCENKUIIEKIABTIXXJCNTH"