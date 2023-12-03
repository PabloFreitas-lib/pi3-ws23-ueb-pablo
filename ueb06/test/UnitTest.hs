module Main where

import Analysis
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain $ testGroup "6. Übungsblatt" [exercise61, exercise62]

exercise61 =
  testGroup "Funktionen und ihre Werte" $
    [ testCase "makePolynomial" $
        makePolynomial [0.7, -3.1, 1.4, 2.1] @?= DegreeN 0.7 (DegreeN (-3.1) (DegreeN 1.4 (Degree0 2.1))),
      testCase "degreeOf_Grad 3" $
        degreeOf (makePolynomial [0.7, -3.1, 1.4, 2.1]) @?= 3,
      testCase "degreeOf_Grad 2" $
        degreeOf (makePolynomial [1, 0, 0]) @?= 2,
      testCase "functionValue" $
        rT 4 (functionValue (makePolynomial [0.7, -3.1, 1.4, 2.1]) 3.5) @?= (-0.9625),
      testCase "valueTable" $
        valueTable (makePolynomial [0.7, -3.1, 1.4, 2.1]) (-1) 2 0.5
          @?= "       x |     f(x)\n-------------------\n    -1.0 |     -3.1\n    -0.5 |    0.537\n     0.0 |      2.1\n     0.5 |    2.113\n     1.0 |      1.1\n     1.5 |   -0.413\n     2.0 |     -1.9\n"
    ]

exercise62 =
  testGroup "Kurvendiskussion" $
    [ testCase "zeros_2 Nullstellen" $
        map (rT 4) (zeros (makePolynomial [2, 3, -4])) @?= [-2.3508, 0.8508],
      testCase "zeros_1 Nullstelle" $
        map (rT 4) (zeros (makePolynomial [0.6, -1.7])) @?= [2.8333],
      testCase "zeros_keine Nullstelle" $
        zeros (makePolynomial [2, 0, 2]) @?= [],
      testCase "yIntercept" $
        yIntercept (makePolynomial [0.7, -3.1, 1.4, 2.1]) @?= 2.1,
      testCase "derivativeOf" $
        derivativeOf (makePolynomial [0.7, -3.1, 1.4, 2.1]) @?= DegreeN 2.1 (DegreeN (-6.2) (Degree0 1.4)),
      testCase "newtonZero" $
        rT 4 (newtonZero (makePolynomial [0.7, -3.1, 1.4, 2.1]) (-0.5)) @?= (-0.5988)
    ]

rT :: Int -> Float -> Float
rT n x = fromIntegral (round (x * 10 ^ n)) / 10 ^ n