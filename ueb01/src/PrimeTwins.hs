module PrimeTwins where
{- 
    Using the mod function to check if a number divides another.
-}
divides :: Int -> Int -> Bool
divides a b = mod b a == 0
{- 
    Here i will use the funciton isPrimeHelp to check if a number is prime.
    Using the basic concept of prime numbers, we can check if a number is prime by checking 
    if it is divisible by any number from 2 to half of the number to be checked.
-}
isPrime :: Int -> Bool
isPrime n = 
    if n < 2 then False
    else isPrimeHelp n (div n 2)
{- 
    The first parameter passed is the number to be checked; the second parameter is the divisor considered in the current recursion step.step.
    We then call this auxiliary function in the isPrime function and run the check recursively with the divisors from 2 to half of the number 
    to be checked (or in the opposite direction, of course). Finally, we can call the function
-}
isPrimeHelp :: Int -> Int -> Bool
isPrimeHelp n d =
    if d == 1 then True
    else if divides d n then False
    else isPrimeHelp n (d-1) 
{- 
    Here we will use the isPrime function to check if the numbers are prime and then check if the difference between them is 2.
-}
isPrimeTwin :: (Int, Int) -> Bool
isPrimeTwin (a, b) = 
    if isPrime a && isPrime b then
        if b - a == 2 then True
        else False
    else False

{-
This function will return a list of tuples with the prime twins. 
-}
allPrimeTwins :: [(Int, Int)]
allPrimeTwins = filter isPrimeTwin [(a, a+2)| a <- [3,5..]]