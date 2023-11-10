module Goldbach where

divides :: Integer -> Integer -> Bool
divides a b = mod b a == 0

isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | otherwise =
      let upper_bound = floor (sqrt (fromIntegral n))
          is_prime_help d = d > upper_bound || not (divides d n) && is_prime_help (d + 1)
       in is_prime_help 2

nextPrime :: Integer -> Integer
nextPrime n
  | isPrime (n + 1) = n + 1
  | otherwise = nextPrime (n + 1)

{--
Extension of list comprehension
 --}
primesUpTo :: Integer -> [Integer]
primesUpTo top =
  [n | n <- [2 .. top], isPrime n]

goldbach :: Integer -> [(Integer, Integer)]
goldbach n =
  [(x, y) | x <- primesUpTo (div n 2), y <- primesUpTo n, x + y == n && x <= y]

{--
This approach is really not good, but it solves the problem.
--}
goldbach2 :: Integer -> (Integer, Integer, Integer)
goldbach2 n = findPair (primesUpTo (div n 2))
  where
    findPair (x : xs) =
      if isPrime (n - x)
        then (n, x, n - x)
        else findPair xs
