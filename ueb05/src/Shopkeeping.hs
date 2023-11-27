module Shopkeeping where

import Data.Char
import Data.List
import GHC.Profiling (stopProfTimer)

type Item = (String, Int)

type ItemList = [Item]

-- Warenlager1
il1 = [("Honey", 5), ("Milk", 14), ("Vinegar", 14), ("Egg", 37)] :: ItemList

target1 = [("Milk", 30), ("Egg", 100), ("Vinegar", 12), ("Honey", 12)] :: ItemList

priceList1 =
  [ ("Milk", 99),
    ("Vinegar", 129),
    ("Egg", 28),
    ("Rice", 149),
    ("Honey", 299),
    ("Juice", 199)
  ] ::
    ItemList

{--
Basic lambda funtion to identify if the product is in the list
--}
isItem :: String -> ItemList -> Bool
isItem product = any (\(itemName, _) -> itemName == product)

{-- Trying something new, using this Maybe type
Following all the other programming languages the Set and Get functions are trivial
here I wanted to try out one to see if would be easy. --}
getElementByName :: String -> ItemList -> Maybe Item
getElementByName name itemList =
  case filter (\(itemName, _) -> itemName == name) itemList of
    [] -> Nothing -- Item not found
    (x : _) -> Just x -- Return the first matching item

{--Direct usage of the sort function, a bit uncessary--}
sortIL :: ItemList -> ItemList
sortIL = sort

{--Concatanation from functions
I tried out the unzip functions and its separates what i needed
sum . map snd (Alternative)
--}
sumIL :: ItemList -> Int
sumIL = sum . snd . unzip

{--Basic string concatanation--}
showItem :: Int -> Item -> String
showItem size (name, amount) =
  "|" ++ take size name ++ replicate (size - length name) ' ' ++ "|" ++ replicate (size - length (show amount)) ' ' ++ show amount ++ "|\n"

{--
This case the problems are located in this 17, this is not generic and i couldn't the
right information from the problem description
--}
showIL :: ItemList -> String
showIL iList =
  replicate 17 '#'
    ++ "\n"
    ++ showILAux iList
    ++ replicate 17 '#'
    ++ "\n"

{-- I cound't make it generic the 7, if this information was given in the text, i missed--}
showILAux :: ItemList -> String
showILAux (x : xs)
  | null xs = showItem 7 x
  | otherwise = showItem 7 x ++ showILAux xs

{--
I preferer to have a lot of "attributes" then compacting everything at one.
At least here i hope its readable
The magic here happened on the zipWith (operator) DATA DATA - nice function to use
The rest was breaking the big problem in small ones
--}
refill :: ItemList -> ItemList -> ItemList
refill target il =
  let sortedTarget = sort target
      sortedIl = sort il
      pricesIl = map snd (sort il)
      pricesTarget = map snd sortedTarget
      pricesDff = zipWith (-) pricesTarget pricesIl
      namesTarget = map fst sortedTarget
      isPositive (_, value) = value > 0
   in filter isPositive (zip namesTarget pricesDff)

{--
At first just a lamda function to be apllied in the map would be enough
but with the second condition i thought of creating the updateItem function and
scalate with the map
--}
sellItem :: Item -> ItemList -> ItemList
sellItem item stgL = sort $ map updateItem stgL
  where
    itemName = fst item
    itemQtd = snd item
    updateItem (itemName', value)
      | itemName' == itemName = (itemName', max 0 (value - itemQtd))
      | otherwise = (itemName', value)

{--Iteration in the list calling the sellItem--}
sellIL :: ItemList -> ItemList -> ItemList
sellIL (x : xs) il
  | null xs = sellItem x il
  | otherwise = sellIL xs (sellItem x il)

{-- Breking the problems in small pieces, but here there are Itens which doesnt exist in the list
What shoudl i do then?
Apply a filter in the priceList, to only use the zipWith with elements included in the iL--}
invoiceIL :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
invoiceIL priceList il =
  let sortedPriceList = sort priceList
      sortedIl = sort il
      filteredPriceList = filter (\(item, _) -> item `elem` map fst sortedIl) sortedPriceList
      pricesPriceList = take (length il) (map snd filteredPriceList)
      pricesIl = map snd sortedIl
      pricesMultiply = zipWith (*) pricesPriceList pricesIl
      namesIl = map fst sortedIl
   in zip namesIl pricesMultiply

-- Warenlager2
il2 =
  [ ("Ajvar", 5),
    ("Amaranth", 7),
    ("Ananas", 5),
    ("Apfelmus", 16),
    ("Azukibohnen", 5),
    ("Baguette", 11),
    ("Bambussprossen", 0),
    ("BarbecueSauce", 12),
    ("Berberitzen", 6),
    ("BlueCuracao", 10),
    ("Brechbohnen", 18),
    ("Brotgurken", 14),
    ("Butter", 15),
    ("Caramel", 4),
    ("Cevapcici", 2),
    ("CocktailSauce", 7),
    ("Cola", 24),
    ("Cookies", 5),
    ("Couscous", 18),
    ("CremeBrulee", 7),
    ("Curry", 24),
    ("Dinkelflocken", 20),
    ("Donut", 4),
    ("Druckerpapier", 4),
    ("Ei", 42),
    ("EmeraldEnergy", 8),
    ("Espresso", 9),
    ("Fischsauce", 3),
    ("Fuellfederhalter", 2),
    ("Gemuesefond", 4),
    ("Gewuerzgurken", 21),
    ("Haehnchen", 3),
    ("Haferflocken", 17),
    ("HollondaiseSauce", 14),
    ("Honig", 12),
    ("Kaese", 12),
    ("Kaffee", 5),
    ("Kakao", 0),
    ("Kamillentee", 8),
    ("Kapern", 8),
    ("Kardamom", 5),
    ("Karotte", 24),
    ("Kichererbsen", 8),
    ("Kokosmilch", 6),
    ("Koriander", 8),
    ("Mais", 24),
    ("Margarine", 21),
    ("Marmelade", 9),
    ("Mehl", 32),
    ("Melone", 0),
    ("Milch", 19),
    ("Muffins", 8),
    ("Mungobohnen", 0),
    ("NatureWater", 64),
    ("OliveOil", 12),
    ("Orangensaft", 12),
    ("Paranusskerne", 14),
    ("Pfeffer", 16),
    ("Pizza", 13),
    ("Pralinen", 18),
    ("Quark", 35),
    ("Reis", 38),
    ("Salami", 12),
    ("Salat", 20),
    ("Salatgurken", 29),
    ("Salz", 14),
    ("Sauerkirschen", 10),
    ("Sauerkraut", 9),
    ("Scharzkuemmel", 5),
    ("Schinken", 14),
    ("Schokolade", 21),
    ("SchokoladeZB", 10),
    ("Schokoladeneis", 2),
    ("Senf", 8),
    ("Sesamoel", 4),
    ("Sojabohnen", 8),
    ("Sojasauce", 4),
    ("Sonntagsbroetchen", 53),
    ("Spaghetti", 22),
    ("SparklingWater", 86),
    ("Spiralnudeln", 20),
    ("SquirrelSnacks", 14),
    ("Tofu", 10),
    ("TomatenGetrocknet", 8),
    ("Vanilleeis", 4),
    ("Vanillekeks", 1),
    ("Vanilleschote", 2),
    ("Wassermelone", 4),
    ("Zahnbuerste", 8),
    ("Zimt", 6)
  ] ::
    ItemList

target2 =
  [ ("Ajvar", 8),
    ("Amaranth", 10),
    ("Ananas", 12),
    ("Apfelmus", 20),
    ("Azukibohnen", 10),
    ("Baguette", 30),
    ("Bambussprossen", 8),
    ("BarbecueSauce", 10),
    ("Berberitzen", 8),
    ("BlueCuracao", 14),
    ("Brechbohnen", 16),
    ("Brotgurken", 14),
    ("Butter", 30),
    ("Caramel", 6),
    ("Cevapcici", 12),
    ("CocktailSauce", 7),
    ("Cola", 20),
    ("Cookies", 40),
    ("Couscous", 20),
    ("CremeBrulee", 7),
    ("Curry", 20),
    ("Dinkelflocken", 20),
    ("Donut", 16),
    ("Druckerpapier", 4),
    ("Ei", 100),
    ("EmeraldEnergy", 24),
    ("Espresso", 6),
    ("Fischsauce", 4),
    ("Fuellfederhalter", 2),
    ("Gemuesefond", 6),
    ("Gewuerzgurken", 25),
    ("Haehnchen", 8),
    ("Haferflocken", 20),
    ("HollondaiseSauce", 16),
    ("Honig", 12),
    ("Kaese", 39),
    ("Kaffee", 10),
    ("Kakao", 8),
    ("Kamillentee", 10),
    ("Kapern", 9),
    ("Kardamom", 6),
    ("Karotte", 70),
    ("Kichererbsen", 12),
    ("Kokosmilch", 12),
    ("Koriander", 8),
    ("Mais", 20),
    ("Margarine", 30),
    ("Marmelade", 12),
    ("Mehl", 40),
    ("Melone", 10),
    ("Milch", 24),
    ("Muffins", 10),
    ("Mungobohnen", 4),
    ("NatureWater", 120),
    ("OliveOil", 12),
    ("Orangensaft", 12),
    ("Paranusskerne", 16),
    ("Pfeffer", 15),
    ("Pizza", 20),
    ("Pralinen", 10),
    ("Quark", 60),
    ("Reis", 42),
    ("Salami", 15),
    ("Salat", 20),
    ("Salatgurken", 40),
    ("Salz", 20),
    ("Sauerkirschen", 10),
    ("Sauerkraut", 12),
    ("Scharzkuemmel", 7),
    ("Schinken", 20),
    ("Schokolade", 30),
    ("SchokoladeZB", 10),
    ("Schokoladeneis", 8),
    ("Senf", 12),
    ("Sesamoel", 6),
    ("Sojabohnen", 10),
    ("Sojasauce", 6),
    ("Sonntagsbroetchen", 90),
    ("Spaghetti", 30),
    ("SparklingWater", 120),
    ("Spiralnudeln", 20),
    ("SquirrelSnacks", 17),
    ("Tofu", 10),
    ("TomatenGetrocknet", 8),
    ("Vanilleeis", 7),
    ("Vanillekeks", 8),
    ("Vanilleschote", 3),
    ("Wassermelone", 12),
    ("Zahnbuerste", 10),
    ("Zimt", 10)
  ] ::
    ItemList

priceList2 =
  [ ("Ajvar", 129),
    ("Amaranth", 209),
    ("Ananas", 239),
    ("Apfel", 50),
    ("Apfelmus", 89),
    ("Azukibohnen", 269),
    ("Baguette", 60),
    ("BalsamicoEssig", 249),
    ("Bambussprossen", 139),
    ("BarbecueSauce", 149),
    ("Berberitzen", 399),
    ("Besen", 1099),
    ("BlueCuracao", 379),
    ("Brechbohnen", 229),
    ("Brotgurken", 149),
    ("Butter", 129),
    ("Caramel", 899),
    ("Cevapcici", 269),
    ("CocktailSauce", 229),
    ("Cola", 370),
    ("Cookies", 30),
    ("Couscous", 199),
    ("CremeBrulee", 549),
    ("Curry", 219),
    ("Dinkelflocken", 129),
    ("Donut", 99),
    ("Druckerpapier", 299),
    ("Ei", 28),
    ("EiXL", 38),
    ("EmeraldEnergy", 379),
    ("Espresso", 1499),
    ("Fischsauce", 299),
    ("Fuellfederhalter", 1199),
    ("Gemuesefond", 250),
    ("Gewuerzgurken", 259),
    ("Haehnchen", 600),
    ("Haferflocken", 79),
    ("HollondaiseSauce", 129),
    ("Honig", 295),
    ("Kaese", 79),
    ("Kaffee", 599),
    ("Kakao", 299),
    ("Kamillentee", 49),
    ("Kapern", 170),
    ("Kardamom", 249),
    ("Karotte", 19),
    ("Kichererbsen", 69),
    ("Kokosmilch", 149),
    ("Koriander", 249),
    ("Mais", 75),
    ("Mandelkerne", 399),
    ("Margarine", 59),
    ("Marmelade", 270),
    ("Mehl", 149),
    ("Melone", 299),
    ("Milch", 99),
    ("Muffins", 50),
    ("Mungobohnen", 279),
    ("NatureWater", 15),
    ("OliveOil", 230),
    ("Orangensaft", 179),
    ("Paranusskerne", 599),
    ("Pfeffer", 249),
    ("Pizza", 50),
    ("Pralinen", 549),
    ("Quark", 49),
    ("Reis", 80),
    ("Salami", 159),
    ("Salat", 79),
    ("Salatgurken", 299),
    ("Salz", 259),
    ("Sauerkirschen", 269),
    ("Sauerkraut", 79),
    ("Scharzkuemmel", 229),
    ("Schinken", 149),
    ("Schokolade", 99),
    ("SchokoladeZB", 99),
    ("Schokoladeneis", 149),
    ("Senf", 99),
    ("Sesamoel", 319),
    ("Sojabohnen", 229),
    ("Sojasauce", 120),
    ("Sonntagsbroetchen", 25),
    ("Spaghetti", 89),
    ("SparklingWater", 25),
    ("Spiralnudeln", 169),
    ("SquirrelSnacks", 20),
    ("Tofu", 140),
    ("TomatenGetrocknet", 199),
    ("Vanilleeis", 149),
    ("Vanillekeks", 25),
    ("Vanilleschote", 499),
    ("Wassermelone", 149),
    ("Zahnbuerste", 105),
    ("Zimt", 399)
  ] ::
    ItemList
