module Family where

import Control.Applicative ((<|>))
import Data.List
import Data.Maybe
import GHC.Exception (underflowException)
import GHC.Real (underflowError)
import VarTree

type Id = Int

type Year = Int

data Person = Person
  { getId :: Id,
    getName :: String,
    getYearOfBirth :: Year,
    getYearOfDeath :: Maybe Year,
    getParents :: (Maybe Id, Maybe Id),
    getPartners :: [Id],
    getChildren :: [Id],
    getRoyalState :: RoyalState
  }
  deriving (Eq, Show)

type Family = [Person]

getPerson :: Family -> Id -> Person
getPerson fam id = fromMaybe (error $ "Person " ++ show id ++ " not found!") $ find (\p -> getId p == id) fam

data RoyalState
  = Ruler
  | RoyalFamily
  | NonRoyal
  deriving (Eq, Show)

-- 7.1 Personen
-- ############

{-
  Eingabe: Year (Jahr, an dem das Alter abgefragt wird), Person (Person, von der man das Alter wissen möchte)
  Ausgabe: Year (Das Alter der Person im eingegebenen Jahr)
  Gibt das Alter einer Person im eingegebenen Jahr aus.
-}
getAge :: Year -> Person -> Year
getAge y p
  | isJust (getYearOfDeath p) = fromJust (getYearOfDeath p) - getYearOfBirth p
  | otherwise = y - getYearOfBirth p

---------------------------------------------------------------------------------
{-
  Eingabe: Year (Jahr, an dem der Zustand der Person geprüft wird), Person (Person, von der man den Zustand wissen möchte)
  Ausgabe: Bool (Ist die Person noch am Leben?)
  Gibt aus, ob die eingegebene Person im eingegebenen Jahr noch lebt oder nicht.
-}
isAlive :: Year -> Person -> Bool
isAlive y p
  | isNothing (getYearOfDeath p) = True
  | isJust (getYearOfDeath p) = y - fromJust (getYearOfDeath p) <= 0
  | otherwise = False

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der Person), Year (Jahr, an dem geprüft wird), Person (Ist diese Person ein Waisenkind?)
  Ausgabe: Bool (Ist die eingegebene Person ein Waisenkind?)
  Gibt aus, ob die eingegebene Person ein Waisenkind ist oder nicht. Personen ab dem Alter von 18 können keine Waisenkinder sein.
-}
isOrphan :: Family -> Year -> Person -> Bool
isOrphan f y p
  | getAge y p < 18 = not $ isInFamily f (getParents p)
  | otherwise = False

{-
  Eingabe: Family (Familie der Person), Eltern-Tupel (Eltern-Tupel, welches geprüft wird)
  Ausgabe: Bool (Sind die Eltern in der Family?)
  Schaut, ob sich die eingegebenen Eltern in der Family befinden.
-}
isInFamily :: Family -> (Maybe Id, Maybe Id) -> Bool
isInFamily [] _ = False
isInFamily f parents
  | isJust (fst parents) && isJust (snd parents) = fst parents `elem` map (fst . getParents) f || snd parents `elem` map (snd . getParents) f
  | isNothing (fst parents) && isJust (snd parents) = snd parents `elem` map (snd . getParents) f
  | isJust (fst parents) && isNothing (snd parents) = fst parents `elem` map (fst . getParents) f
  | isNothing (fst parents) && isNothing (snd parents) = False

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der Person), Person (Person, von der man die Geschwister wissen möchte)
  Ausgabe: Person-Liste (Liste mit allen Geschwistern der eingegebenen Person)
  Gibt eine Liste mit allen Geschwistern der eingegebenen Person aus.
-}
getSiblings :: Family -> Person -> [Person]
getSiblings f p = filter (/= p) (map (\x -> f !! (x - 1)) (getChildrenOutOfList (getParentsList f p)))

{-
  Eingabe: Person-Liste (Eltern der Person, von der man die Geschwister wissen möchte)
  Ausgabe: Id-Liste (Liste mit allen Ids der Geschwister der Person)
  Gibt eine Liste aller Geschwister der Person, von der man die Geschwister in der Funktion getSiblings wissen möchte, aus.
-}
getChildrenOutOfList :: [Person] -> [Id]
getChildrenOutOfList parents = nub (concatMap getChildren parents)

{-
  Eingabe: Family (Familie der Person), Person (Person, von der man die Eltern wissen möchte)
  Ausgabe: Person-Liste (Liste mit den Eltern der eingegebenen Person)
  Gibt eine maximal zwei große Liste mit den Eltern der eingegebenen Person aus.
-}
getParentsList :: Family -> Person -> [Person]
getParentsList [] _ = []
getParentsList f p
  | isJust (fst (getParents p)) && isJust (snd (getParents p)) = [getPerson f (fromJust (fst (getParents p))), getPerson f (fromJust (snd (getParents p)))]
  | isNothing (fst (getParents p)) && isJust (snd (getParents p)) = [getPerson f (fromJust (snd (getParents p)))]
  | isJust (fst (getParents p)) && isNothing (snd (getParents p)) = [getPerson f (fromJust (fst (getParents p)))]
  | isNothing (fst (getParents p)) && isNothing (snd (getParents p)) = []

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der Person), Person (Person, von der man die Halb-Geschwister wissen möchte)
  Ausgabe: Person-Liste (Liste mit allen Halb-Geschwistern der eingegebenen Person)
  Gibt eine Liste mit allen Halb-Geschwistern der eingegebenen Person aus.
-}
getHalfSiblings :: Family -> Person -> [Person]
getHalfSiblings f p = filter (\x -> getBothVersions (getParents x) /= getBothVersions (getParents p)) (getSiblings f p)

{-
  Eingabe: Eltern-Tupel (Eltern der eingegebenen Person in der Funktion getHalfSiblings)
  Ausgabe: Maybe-Id-Liste (Liste, worin die eingegebenen Eltern sortiert enthalten sind)
  Gibt eine Liste mit den eingegebenen Eltern sortiert aus.
-}
getBothVersions :: (Maybe Id, Maybe Id) -> [Maybe Id]
getBothVersions (x, y) = sort [x, y]

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der Person), Person (Person, von der man die leiblichen Geschwister wissen möchte)
  Ausgabe: Person-Liste (Liste mit allen leiblichen Geschwistern der eingegebenen Person)
  Gibt eine Liste mit allen leiblichen Geschwistern der eingegebenen Person aus.
-}
getFullSiblings :: Family -> Person -> [Person]
getFullSiblings f p = filter (\x -> x `notElem` getHalfSiblings f p) (getSiblings f p)

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der Person), Person (Person, von der man die Onkeln und Tanten wissen möchte)
  Ausgabe: Person-Liste (Liste mit allen Onkeln und Tanten der eingegebenen Person)
  Gibt eine Liste mit allen Onkeln und Tanten der eingegebenen Person aus.
-}
getSiblingsOfParents :: Family -> Person -> [Person]
getSiblingsOfParents f p = getSiblingsOfParents2 f (getParentsList f p)

{-
  Eingabe: Family (Familie der Person), Person-Liste (Eltern der eingegebenen Person in der Funktion getSiblingsOfParents)
  Ausgabe: Person-Liste (Liste mit allen Onkeln und Tanten der eingegebenen Person)
  Gibt eine Liste mit allen Onkeln und Tanten der eingegebenen Person aus.
  Hilfsfunktion von der Funktion getSiblingsOfParents
-}
getSiblingsOfParents2 :: Family -> [Person] -> [Person]
getSiblingsOfParents2 f parents
  | length parents == 2 = getSiblings f (head parents) ++ getSiblings f (parents !! 1)
  | otherwise = getSiblings f (head parents)

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der Person), Person (Person, von der man die Cousins wissen möchte)
  Ausgabe: Person-Liste (Liste mit allen Cousins der eingegebenen Person)
  Gibt eine Liste mit Cousins und Cousinen der eingegebenen Person aus.
-}
getCousins :: Family -> Person -> [Person]
getCousins f p = map (\x -> f !! (x - 1)) (concatMap getChildren (getSiblingsOfParents f p))

---------------------------------------------------------------------------------
-- Funktionen, welche nicht gebraucht werden, aber zur Veranschaulichung der Listen beitragen könnten:

{-
  Eingabe: Personen-Liste (Liste mit beliebigen Personen)
  Ausgabe: String-Liste (Namen der eingegebenen Personen)
  Gibt eine Liste von allen Namen der Personen aus (Besser zum Lesen :)).
-}
inNames :: [Person] -> [String]
inNames = map getName

-- 7.2 Stamm**baum**
-- #################

{-
  Eingabe: Family (Familie der eingegebenen Person), Person (Person, von der man die Nachfahren wissen möchte)
  Ausgabe: VarTree-Id (Baum mit allen Nachfahren der Person)
  Gibt einen Baum mit allen Nachfahren der Person aus.
-}
getDescendants :: Family -> Person -> VarTree Id
getDescendants family person =
  let children = getChildren person
      childrenTrees = map (getDescendants family . getPerson family) children
   in VarTree (getId person) childrenTrees

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der eingegebenen Person), Person (Person, von der man die Nachfahren wissen möchte)
  Ausgabe: Person-Liste (Liste mit allen Nachfahren der eingegebenen Person)
  Gibt eine Liste mit allen Nachfahren der Person aus.
-}
getDescendantPersons :: Family -> Person -> [Person]
getDescendantPersons family person =
  let children = getChildren person
      descendantChildren = concatMap (getDescendantPersons family . getPerson family) children
   in nub $ person : descendantChildren

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der eingegebenen Person), Person (Person, von der man die Vorfahren wissen möchte)
  Ausgabe: VarTree-Id (Baum mit allen Vorfahren der Person)
  Gibt einen Baum mit allen Vorfahren der Person aus.
-}
getAncestors :: Family -> Person -> VarTree Id
getAncestors family person
  | isNothing (fst $ getParents person) && isNothing (snd $ getParents person) = VarTree (getId person) []
  | otherwise =
      VarTree
        (getId person)
        [ getAncestors family (getPerson family (fromJust (fst $ getParents person))),
          getAncestors family (getPerson family (fromJust (snd $ getParents person)))
        ]

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der eingegebenen Person), Person (Person, von der man die Vorfahren wissen möchte)
  Ausgabe: Person-Liste (Liste mit allen Vorfahren der eingegebenen Person)
  Gibt eine Liste mit allen Vorfahren der Person aus.
-}
getAncestorPersons :: Family -> Person -> [Person]
getAncestorPersons family person
  | isNothing (fst $ getParents person) && isNothing (snd $ getParents person) = [person]
  | otherwise =
      person
        : concatMap (getAncestorPersons family) ancestors
  where
    ancestors =
      map (getPerson family) $
        catMaybes [fst $ getParents person, snd $ getParents person]

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie des Familenbaums), VarTree-Id (Baum mit allen Vor- und Nachfahren einer Person (IDs))
  Ausgabe: VarTree-String (Baum mit allen Vor- und Nachfahren einer Person (Namen))
  Gibt einen Familenbaum mit allen Namen der Vor- und Nachfahren einer Person aus.
-}
convertToNames :: Family -> VarTree Id -> VarTree String
convertToNames family = fmap (getName . getPerson family)

---------------------------------------------------------------------------------
{-
  Eingabe: Person-Liste (Famile mit möglichen nicht verwandten der eingegebenen Person), Person (Person, von der man die wirklichen Verwandten wissen möchte)
  Ausgabe: Family (Familie mit allen wirklichen Verwandten der eingegebenen Person)
  Gibt eine Familie als Liste aus, welche alle wirklichen Verwandten der eingegebenen Person enthält.
-}
getFamily :: [Person] -> Person -> Family
getFamily family person =
  let descendants = getDescendantPersons family person
      ancestors = getAncestorPersons family person
      siblings = getSiblings family person
      cousins = getCousins family person
      halfSiblings = getHalfSiblings family person
      fullSiblings = getFullSiblings family person
      siblingsOfParents = getSiblingsOfParents family person
   in nub $
        [person]
          ++ concatMap (getFamily family . getPerson family) (getChildren person)
          ++ siblings
          ++ cousins
          ++ halfSiblings
          ++ fullSiblings
          ++ siblingsOfParents
          ++ descendants
          ++ ancestors

---------------------------------------------------------------------------------
{-
  Eingabe: Person (Person, welche überarbeitet worden ist und in der Familie ausgetauscht werden soll), Family (Familie, welche geupdated werden soll)
  Ausgabe: Family (Familie, nachdem sie mit der eingegebenen Person geupdated worden ist)
  Updated eine Person in der Familie, indem sie die eingegebene Person mit der gleichen Person in der Familien-Liste austauscht.
-}
update :: Person -> Family -> Family
update updatedPerson family =
  let familyWithoutPerson = filter (\p -> getId p /= getId updatedPerson) family
   in sortOn getName $ updatedPerson : familyWithoutPerson

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der Eltern), String (Name des Kindes), Year (Jahr, an dem die Familie aktualisiert wird), Person (Mutter), Person (Vater)
  Ausgabe: Family (Aktualisierte Familie)
  Gibt eine aktualisierte Familie aus, wo die Kinder der Eltern aktualisiert sind.
-}
conceiveChild :: Family -> String -> Year -> Person -> Person -> Family
conceiveChild family name year mother father =
  let nextId = maximum (map getId family) + 1
      royalState = if getRoyalState mother == Ruler || getRoyalState mother == RoyalFamily || getRoyalState father == Ruler || getRoyalState father == RoyalFamily then RoyalFamily else NonRoyal
      child = Person nextId name year Nothing (Just $ getId mother, Just $ getId father) [] [] royalState
      updateMother = mother {getChildren = getChildren mother ++ [nextId]}
      updateFather = father {getChildren = getChildren father ++ [nextId]}
      updatedFamily = map (\p -> if getId p == getId mother then updateMother else if getId p == getId father then updateFather else p) family
   in sortOn getName $ child : updatedFamily

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der beiden Personen), Person (Erste Person), Person (Zweite Person)
  Ausgabe: Family (Aktualisierte Familie)
  Gibt eine aktualisierte Familie aus, nachdem die beiden Personen geheiratet haben.
-}
marry :: Family -> Person -> Person -> Family
marry family person1 person2 =
  let familyWithoutPerson1 = filter (\p -> getId p /= getId person1) family
      familyWithoutPerson2 = filter (\p -> getId p /= getId person2) familyWithoutPerson1
      person1WithNewPartner = person1 {getPartners = getPartners person1 ++ [getId person2]}
      person2WithNewPartner = person2 {getPartners = getPartners person2 ++ [getId person1]}
   in nub $ person1WithNewPartner : person2WithNewPartner : familyWithoutPerson2

-- 7.3 Thronfolge
-- ##############
{-
  Eingabe: Family (Familie der eingegebenen Person), Year (Jahr, an dem geprüft wird), Person (Person, von der der Nachfahre bestimmt werden soll)
  Ausgabe: Maybe-Person (Der nächste Nachfahre der eingegebenen Person)
  Gibt den nächsten Nachfahren der eingegebenen Person aus oder falls es den nicht gibt, Nothing aus.
-}
getFirstLivingDescendant :: Family -> Year -> Person -> Maybe Person
getFirstLivingDescendant family year person =
  let children = getChildren person
      childrenSorted = sortOn getYearOfBirth (map (getPerson family) children)
      childrenSortedAlive = filter (isAlive year) childrenSorted
   in if null childrenSortedAlive then getFirstLivingDescendant2 family year childrenSorted else Just (head childrenSortedAlive)

{-
  Eingabe: Family (Familie der eingegebenen Person), Year (Jahr, an dem geprüft wird), Person-Liste (Liste mit allen Kindern der Person in der Funktion getFirstLivingDescendant (sortiert))
  Ausgabe: Maybe-Person (Der nächste Nachfahre der eingegebenen Person)
  Gibt den nächsten Nachfahren der eingegebenen Person aus oder falls es den nicht gibt, Nothing aus.
  Hilfsfunktion von der Funktion getFirstLivingDescendant. Durchläuft alle Kinder der Kinder.
-}
getFirstLivingDescendant2 :: Family -> Year -> [Person] -> Maybe Person
getFirstLivingDescendant2 _ _ [] = Nothing
getFirstLivingDescendant2 family year childrenSorted = getFirstLivingDescendant family year (head childrenSorted)

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der eingegebenen Person), Year (Jahr, an dem geprüft wird), Person (Person, von der der Nachfahre bestimmt werden soll)
  Ausgabe: Maybe-Person (Der nächste Nachfahre der eingegebenen Person)
  Gibt den nächsten Nachfahren der eingegebenen Person aus, indem er die Vorfahren mit einbezieht und falls kein direkter Nachfahre da ist, einen anderen passenden aus der Famile findet.
  Falls es die Person dennoch nicht gibt, gibt diese Funktion Nothing aus.
-}
getFirstLivingSuccessor :: Family -> Year -> Person -> Maybe Person
getFirstLivingSuccessor family year person
 | isAlive year person = Just person
 | isJust (getFirstLivingDescendant family year person) = getFirstLivingDescendant family year person
 | otherwise =
  let parents = getParentsList family person
   in case length parents of
    0 -> Nothing
    1 -> if isAlive year (head parents) then Just $ head parents else getFirstLivingDescendant family year (head parents)
    2 -> case isAlive year (head parents) of
      True -> Just $ head parents
      False ->
        if isAlive year (parents !! 1) then Just $ parents !! 1 else
        if isNothing (getFirstLivingDescendant family year (head parents)) then getFirstLivingDescendant family year (parents !! 1)
        else getFirstLivingDescendant family year (head parents)

---------------------------------------------------------------------------------
{-
  Eingabe: Family (Familie der eingegebenen Person), Year (Jahr, an dem geprüft wird)
  Ausgabe: Family (Aktualisierte Familie mit einem neuen Ruler)
  Aktualisiert die Familie mit einem neuen Ruler.
-}
updateRuler :: Family -> Year -> Family
updateRuler family year =
  let successor = getFirstLivingSuccessor family year (fromJust $ find (\x -> getRoyalState x == Ruler) family)
  in case isNothing successor of
    True -> family
    False ->
        let oldRuler = (fromJust $ find (\x -> getRoyalState x == Ruler) family) {getRoyalState = RoyalFamily}
          in update ((fromJust (successor)){getRoyalState = Ruler}) (update oldRuler family)

-- Beispielfamilien
-- ################

family = [alex, berta, clemens, dieter, erna, friederike, gustav, hannes, irma, jan, katja, luca]

alex = Person 1 "Alex" 2000 Nothing (Just $ getId clemens, Just $ getId berta) [getId irma] [getId luca] RoyalFamily

berta = Person 2 "Berta" 1970 Nothing (Just $ getId dieter, Just $ getId erna) [getId clemens] [getId alex, getId jan, getId katja] RoyalFamily

clemens = Person 3 "Clemens" 1970 (Just 2021) (Just $ getId friederike, Just $ getId gustav) [getId berta] [getId alex, getId katja] NonRoyal

dieter = Person 4 "Dieter" 1940 (Just 2020) (Nothing, Nothing) [getId erna] [getId berta, getId hannes] Ruler

erna = Person 5 "Erna" 1942 Nothing (Nothing, Nothing) [getId dieter] [getId berta, getId hannes] NonRoyal

friederike = Person 6 "Friderike" 1945 Nothing (Nothing, Nothing) [getId gustav] [getId clemens] NonRoyal

gustav = Person 7 "Gustav" 1946 Nothing (Nothing, Nothing) [getId friederike] [getId clemens] NonRoyal

hannes = Person 8 "Hannes" 1968 Nothing (Just $ getId dieter, Just $ getId erna) [] [getId irma] RoyalFamily

irma = Person 9 "Irma" 1998 Nothing (Just $ getId hannes, Nothing) [getId alex] [getId luca] RoyalFamily

jan = Person 10 "Jan" 2002 Nothing (Just $ getId berta, Nothing) [] [] RoyalFamily

katja = Person 11 "Katja" 1999 Nothing (Just $ getId berta, Just $ getId clemens) [] [] RoyalFamily

luca = Person 12 "Luca" 2022 Nothing (Just $ getId irma, Just $ getId alex) [] [] RoyalFamily

family2 = [abel, barbara, christoph]

abel = Person 21 "Abel" 1950 Nothing (Nothing, Nothing) [] [getId christoph] NonRoyal

barbara = Person 22 "Barbara" 1950 Nothing (Nothing, Nothing) [] [getId christoph] NonRoyal

christoph = Person 23 "Christoph" 1975 (Just 2021) (Just $ getId abel, Just $ getId barbara) [] [] NonRoyal
