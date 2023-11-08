module ListUtils where

import GHC.Read (list)

{--
Ref:
containsDoubleElem " Hallo ! " ⇝ True containsDoubleElem [ 1 , 2 , 1 ] ⇝ False
Start using the guards from the first correction, lets see if we can do better.
(head:tail) is a pattern for not empty list to extract the head and the tail of the list.
Here there is a function called elem to check if an element is in a list. (Why is my compiler recommending the index notation?)
--}
containsDoubleElem :: (Eq a) => [a] -> Bool
containsDoubleElem [] = False
containsDoubleElem (hElem : tList)
  | null tList = False
  | hElem == head tList = True
  | otherwise = containsDoubleElem tList

getDoubleElemIndex :: (Eq a) => [a] -> Int
getDoubleElemIndex [] = -1
getDoubleElemIndex (hElem : tList)
  | hElem == head tList = 0
  | otherwise = 1 + getDoubleElemIndex tList

removeDoubleElem :: (Eq a) => [a] -> [a]
removeDoubleElem [] = []
removeDoubleElem list
  | containsDoubleElem list = removeDoubleElem (removeElemByIndex (getDoubleElemIndex list) list)
  | otherwise = list

{--
Auxiliar function to remove an element by index
--}
removeElemByIndex :: Int -> [a] -> [a]
removeElemByIndex _ [] = []
removeElemByIndex index (hElem : tList)
  | index == 0 = tList
  | otherwise = hElem : removeElemByIndex (index - 1) tList

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome str = str == reverse str

computeTip :: [Float] -> [(Float, Int)]
computeTip = undefined