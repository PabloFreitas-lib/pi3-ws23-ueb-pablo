module UnitTest where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as M
import PuzzleGame

main = defaultMain $ testGroup "8. Übungsblatt" [exercise81, exercise82, exercise83]

field = createField 5 3
field2 = placeRoad field (3,0) (3,2)
field3 = Field { tiles = M.fromList [((0,0),Grass),((1,0),Road),((0,1),Goal),((1,1),Player East)] }

exercise81 = testGroup "8.1 Field generation" $ [
   testCase "createField" $
    field @?= Field { tiles = M.fromList [((0,0),Grass),((0,1),Grass),((0,2),Grass),((1,0),Grass),((1,1),Grass),((1,2),Grass),((2,0),Grass),((2,1),Grass),((2,2),Grass),((3,0),Grass),((3,1),Grass),((3,2),Grass),((4,0),Grass),((4,1),Grass),((4,2),Grass)] },
   testCase "placeRoad" $
    placeRoad field (3,0) (3,2) @?= Field { tiles = M.fromList [((0,0),Grass),((0,1),Grass),((0,2),Grass),((1,0),Grass),((1,1),Grass),((1,2),Grass),((2,0),Grass),((2,1),Grass),((2,2),Grass),((3,0),Road),((3,1),Road),((3,2),Road),((4,0),Grass),((4,1),Grass),((4,2),Grass)]},
    testCase "placePlayer" $
    placePlayer field2 (3,0) South @?= Field { tiles = M.fromList [((0,0),Grass),((0,1),Grass),((0,2),Grass),((1,0),Grass),((1,1),Grass),((1,2),Grass),((2,0),Grass),((2,1),Grass),((2,2),Grass),((3,0),Player South),((3,1),Road),((3,2),Road),((4,0),Grass),((4,1),Grass),((4,2),Grass)]},
   testCase "placeEnemy" $
   placeEnemy field2 (3,1) North @?= Field { tiles = M.fromList [((0,0),Grass),((0,1),Grass),((0,2),Grass),((1,0),Grass),((1,1),Grass),((1,2),Grass),((2,0),Grass),((2,1),Grass),((2,2),Grass),((3,0),Road),((3,1),Enemy North),((3,2),Road),((4,0),Grass),((4,1),Grass),((4,2),Grass)]},
   testCase "placeGoal" $
   placeGoal field2 (3,2) @?= Field { tiles = M.fromList [((0,0),Grass),((0,1),Grass),((0,2),Grass),((1,0),Grass),((1,1),Grass),((1,2),Grass),((2,0),Grass),((2,1),Grass),((2,2),Grass),((3,0),Road),((3,1),Road),((3,2),Goal),((4,0),Grass),((4,1),Grass),((4,2),Grass)]}
   ]

exercise82 = testGroup "8.2 Show" $ [
   testCase "showRoad" $
     show Road @?= " ",
   testCase "showGrass" $
     show Grass @?= "\9632",
   testCase "showGoal" $
     show Goal @?= "X",
   testCase "showPlayer" $
     show (Player South) @?= "\9660",
   testCase "showEnemy" $
     show (Enemy North) @?= "^",
   testCase "fieldFromString" $
     fieldFromString "■ \nX►\n" @?= field3 ,
   testCase "fieldToString" $
     fieldToString field3 @?= "■ \nX►\n"
  ]

exercise83 = testGroup "8.3 Move" $ [
   testCase "movePlayer" $
   movePlayer (Field { tiles = M.fromList [((0,0),Player East),((1,0),Road),((2,0),Goal)] } ) East @?= (Field { tiles = M.fromList [((0,0),Road),((1,0),Player East),((2,0),Goal)] }, Playing),
   testCase "moveEnemies" $
   moveEnemies (Field { tiles = M.fromList [((0,0),Enemy East),((1,0),Road),((0,1),Road),((1,1),Enemy West)] }) @?= (Field { tiles = M.fromList [((0,0),Road),((1,0),Enemy East),((0,1),Enemy West),((1,1),Road)] },Playing)
   ]
