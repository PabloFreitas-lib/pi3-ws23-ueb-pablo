module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Shopkeeping

main = defaultMain $ testGroup "5. Übungsblatt" [exercise51, exercise52]

exercise51 = testGroup "Aufgabe 1" $ [
   testCase "isItem" $
      isItem "Milk" il1 @?= True,
   testCase "sortIL" $
      sortIL il1 @?= [("Egg",37),("Honey",5),("Milk",14),("Vinegar",14)],
   testCase "sumIL" $
      sumIL target1 @?= 154,
   testCase "showItem" $ 
      showItem 8 ("Hallo", 42) @?= "|Hallo   |      42|\n",
   testCase "showIL" $
      showIL target1 @?= "#################\n|Milk   |     30|\n|Egg    |    100|\n|Vinegar|     12|\n|Honey  |     12|\n#################\n"
   ]

exercise52 = testGroup "Aufgabe 2" $ [
   testCase "refill" $ 
      refill target1 il1 @?= [("Egg",63),("Honey",7),("Milk",16)],
   testCase "sellItem" $
      sellItem ("Egg", 4) il1 @?= [("Egg",33),("Honey",5),("Milk",14),("Vinegar",14)],
   testCase "sellIL" $
      sellIL [("Vinegar",3), ("Egg", 10)] il1 @?= [("Egg",27),("Honey",5),("Milk",14),("Vinegar",11)],
   testCase "invoiceIL" $
      invoiceIL priceList1 il1 @?= [("Egg",1036),("Honey",1495),("Milk",1386),("Vinegar",1806)] 
   ]

