module Analysis where

import Data.List
import Text.Printf

data Polynomial
  = Degree0 Float
  | DegreeN Float Polynomial
  deriving (Show, Eq)

-- 1. Polynom aus Liste von Koeffizienten erzeugen

-- Function to make a polynomial from a list of coefficients
makePolynomial :: [Float] -> Polynomial
makePolynomial [] = error "Coefficient list should not be empty."
makePolynomial [c] = Degree0 c
makePolynomial (c : cs) = DegreeN c (makePolynomial cs)

-- generateDegrees n = DegreeN 1.0 (Degree0 (fromIntegral n)) : generateDegrees (n + 1)

-- 2. Grad eines Polynoms ermitteln
degreeOf :: Polynomial -> Int
degreeOf (Degree0 _) = 0
degreeOf (DegreeN _ subPoly) = 1 + degreeOf subPoly

-- 3. Funktionswert an Stelle x berechnen
{-- ax^2 + bx + c = DegreeN a (DegreeN (b) (Degree0 c )) --}
functionValue :: Polynomial -> Float -> Float
functionValue (Degree0 c) _ = c
functionValue (DegreeN c subPoly) x =
  c * x ^ degreeOf (DegreeN c subPoly) + functionValue subPoly x

--- 4. Wertetabelle erstellen
{-- Breaking the problems in line probles was the best for me. It just needs to fit the float to map 1 decimal point --}
valueTable :: Polynomial -> Float -> Float -> Float -> String
valueTable poly start end step =
  "       x |     f(x)\n-------------------\n"
    ++ intercalate "\n" (map (valueTableLine poly) [start, start + step .. end])
    ++ "\n"

valueTableLine :: Polynomial -> Float -> String
valueTableLine poly x =
  printf "%9.1f | %9.3f" x (functionValue poly x)

-- 5. Nullstellen von Polynomen des Grades 1 oder 2
{-- Lets calculate the roots from the functions
The Polynom must be at least 1 or two --}
zeros :: Polynomial -> [Float]
zeros (Degree0 _) = []
zeros (DegreeN c (Degree0 c2)) = [-c2 / c]
zeros (DegreeN c (DegreeN c2 (Degree0 c3))) = solveQuadratic c c2 c3
zeros _ = error "Polynomial must be of degree 1 or 2."

solveQuadratic :: Float -> Float -> Float -> [Float]
solveQuadratic a b c =
  let d = b ^ 2 - 4 * a * c
   in if d < 0
        then []
        else
          sort
            [ (-b + sqrt d) / (2 * a),
              (-b - sqrt d) / (2 * a)
            ]

-- 6. Y-Achsenabschnitt eines Polynoms ermitteln

yIntercept :: Polynomial -> Float
yIntercept (Degree0 c) = c
yIntercept (DegreeN c _) = c

-- 7. Ableitung eines Polynoms bilden

derivativeOf :: Polynomial -> Polynomial
derivativeOf (Degree0 _) = Degree0 0
derivativeOf (DegreeN c subPoly) = DegreeN (c * fromIntegral (degreeOf subPoly)) (derivativeOf subPoly)

-- 8. Näherungsweise Berechnung von Nullstellen mit Newton-Verfahren

newtonZero :: Polynomial -> Float -> Float
newtonZero poly x0 =
  let x1 = x0 - functionValue poly x0 / functionValue (derivativeOf poly) x0
   in if abs (x1 - x0) < 0.0001
        then x1
        else newtonZero poly x1

-- Beispielpolynome

poly0 = makePolynomial [0.9]

poly1 = makePolynomial [0.6, -1.7]

poly2 = makePolynomial [2, 3, -4]

poly2a = makePolynomial [-0.5, 0, 0]

poly2b = makePolynomial [2, 0, 2]

poly3 = makePolynomial [0.7, -3.1, 1.4, 2.1]