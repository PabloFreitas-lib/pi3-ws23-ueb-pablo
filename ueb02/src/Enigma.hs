module Enigma where

import Data.Char
import Data.List

type Rotor = String

rotor0 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
rotor1 = "JKPQRLYZADGVWXHMNOIEFBSTUC"

{-- 
Using the rotor0 as auxiliar to find the index of the character in the rotor1
The operator !! returns the element in the index of the list
the function indexOfChar returns the index of the character in the rotor0
--}
encodeChar :: Char -> Rotor -> Char
encodeChar char rotor = rotor !! (indexOfChar char rotor0)

{-- 
This is just to move the first character to the end of the string
--}
turnRotor :: Rotor -> Rotor
turnRotor [] = []
turnRotor (first_char:rotor_str) = rotor_str ++ [first_char]
{-- 
This will move the last character to the start of the string
--}
reverseRotor :: Rotor -> Rotor
reverseRotor [] = []
reverseRotor rotor = last rotor : init rotor



{-- Using the turnRotor as auxiliar --}
turnRotorByN :: Rotor -> Int -> Rotor
turnRotorByN rotor n =
    if n == 0 then rotor
    else if n > 0 then turnRotorByN (turnRotor rotor) (n-1)
    else turnRotorByN (reverseRotor rotor) ((abs n)-1)
    
{-- 
The first cycle the rotor will turn with the startTurn and then after the first char will
be encoded and afterwards the rotor will turn with the cycleTurn and so on
--}
enigma :: String -> Rotor -> Int -> Int -> String
enigma msg rotor startTurn cycleTurn =
    if msg == [] then []
    else encodeChar (head msg) (turnRotorByN rotor startTurn) : enigma' (tail msg) (turnRotorByN rotor startTurn) cycleTurn

enigma' :: String -> Rotor -> Int -> String
enigma' msg rotor cycleTurn =
    if msg == [] then []
    else encodeChar (head msg) (turnRotorByN rotor cycleTurn) : enigma' (tail msg) (turnRotorByN rotor cycleTurn) cycleTurn

indexOfChar :: Char -> Rotor -> Int
indexOfChar char rotor = 
    if char == head rotor then 0
    else 1 + indexOfChar char (tail rotor)

decodeChar :: Char -> Rotor -> Char
decodeChar char rotor = rotor0 !! (indexOfChar char rotor)

reverseDecodeChar :: Char -> Rotor -> Char
reverseDecodeChar char rotor = rotor1 !! (indexOfChar char rotor)

deEnigma :: String -> Rotor -> Int -> Int -> String
deEnigma msg rotor startTurn cycleTurn =
    if msg == [] then []
    else deEnigma (init msg) rotor startTurn cycleTurn
    ++
    [decodeChar (last msg) (turnRotorByN rotor (length (init msg) * cycleTurn + startTurn))]

breakEnigma :: String -> Rotor -> String
breakEnigma = undefined

breakEnigmaWithGuess :: String -> Rotor -> String -> String
breakEnigmaWithGuess = undefined


secretMessage = "EQWJTEMXUPICNOLPUNUDJDOWIZJANGKXDJFONQNRQQYGFLMGRCENKUIIEKIABTIXXJCNTH"