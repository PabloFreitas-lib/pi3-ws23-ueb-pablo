module Main where

import Data.List((\\), sortOn)

import Test.Tasty
import Test.Tasty.HUnit

import Family
import VarTree

main = defaultMain $ testGroup "7. Übungsblatt" [exercise71, exercise72, exercise73, exercise74]

exercise71 = testGroup "Infos über Personen" $ [
  testCase "getAge" $
    getAge 2023 luca @?= 1
  , testCase "isAlive" $
    isAlive 2023 dieter @?= False
  , testCase "isOrphan false" $
    isOrphan family 2023 erna @?= False
  ]

exercise72 = testGroup "Verwandte nachgucken" $ [
  testCase "getSiblings" $
    getSiblings family berta @?= [hannes]
  , testCase "getHalfSiblings" $
    getHalfSiblings family alex @?= [jan]
  , testCase "getFullSiblings" $
    getFullSiblings family alex @?= [katja]
  , testCase "getSiblingsOfParents" $
    getSiblingsOfParents family alex @?= [hannes]
  , testCase "getCousins" $
    getCousins family alex @?= [irma]
  ]

exercise73 = testGroup "Verwandschaftsbaum" $ [
  testCase "getDescendants" $
    getDescendants family dieter @?=
    VarTree (getId dieter) [
      VarTree (getId berta) [
          VarTree (getId alex) [
              VarTree (getId luca) []],
          VarTree (getId jan) [],
          VarTree (getId katja) []],
        VarTree (getId hannes) [
          VarTree (getId irma) [
              VarTree (getId luca) []]]]
  , testCase "getAncestors" $
    getAncestors family alex @?=
    alexTree getId
  , testCase "fmap VarTree" $
    fmap getId (alexTree id) @?=
    alexTree getId
  , testCase "foldVarTree via toDot" $
    toDot (alexTree getId) @?=
    "digraph {\n1 -> 3\n3 -> 6\n3 -> 7\n1 -> 2\n2 -> 4\n2 -> 5\n}"
  , testCase "convertToNames" $
    convertToNames family (alexTree getId) @?=
    alexTree getName
  , testCase "getFamily" $
    sortOn getId (getFamily (family ++ family2) alex) @?= sortOn getId family
  ]

alexTree f = VarTree (f alex) [
  VarTree (f clemens) [
      VarTree (f friederike) [],
        VarTree (f gustav) []
      ],
    VarTree (f berta) [
      VarTree (f dieter) [],
        VarTree (f erna) []]]

exercise74 = testGroup "Änderungen vornehmen" $ [
  testCase "update" $
    update jan{getName = "Jannes"} [alex,jan] @?=
    [alex,jan{getName = "Jannes"}]
  , testCase "conceiveChild" $
  -- sortiere nach der ID, damit die Reihenfolge egal ist.
    sortOn getId (conceiveChild (family \\ [luca]) "Luca" 2022
                  irma{getChildren = []}
                  alex{getChildren = []}) @?=
    sortOn getId family
  , testCase "marry" $
    sortOn getId (marry family2 abel barbara) @?=
    sortOn getId [abel{getPartners = [getId barbara]},
                  barbara{getPartners = [getId abel]},
                  christoph]
  , testCase "getFirstLivingDescendant" $
    getFirstLivingDescendant family 2023 dieter @?= Just hannes
  , testCase "getFirstLivingSuccessor" $
    getFirstLivingSuccessor family2 2023 christoph @?= Just abel
  , testCase "updateRuler" $
    updateRuler family 2023 @?=
    [alex, berta, clemens, dieter{getRoyalState = RoyalFamily}, erna, friederike, gustav, hannes{getRoyalState = Ruler}, irma, jan, katja, luca]
  ]
