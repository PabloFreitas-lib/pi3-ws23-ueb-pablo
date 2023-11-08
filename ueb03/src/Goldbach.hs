module Goldbach where


divides :: Integer -> Integer -> Bool
divides a b = mod b a == 0


isPrime :: Integer -> Bool
isPrime n | n < 2     = False
          | otherwise = let upper_bound     = floor (sqrt (fromIntegral n))
                            is_prime_help d = d > upper_bound || not (divides d n) && is_prime_help (d+1) 
                        in is_prime_help 2


nextPrime :: Integer -> Integer
nextPrime = undefined


primesUpTo :: Integer -> [Integer]
primesUpTo = undefined


goldbach :: Integer -> [(Integer, Integer)]
goldbach = undefined


goldbach2 :: Integer -> (Integer, Integer, Integer)
goldbach2 = undefined