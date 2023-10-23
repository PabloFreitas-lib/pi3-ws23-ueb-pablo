module SumISq where

-- Function doc
-- Sum of squares of first n integers
sumISq :: Integer -> Integer
sumISq 0 = 0
sumISq n = n^2 + sumISq (n-1)