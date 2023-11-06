Bewertung des Übungsblattes:

Übung 1.1:   4 von  4 Punkten
Übung 1.2:   6 von  6 Punkten

Insgesamt:  10 von 10 Punkten

Bemerkungen:
if-else statements in Haskell are not pretty when used inside each other, consider using guards instead

It's not always needed to use if statements, e.g.:
isPrimeTwin (a, b) = 
    if isPrime a && isPrime b then
        if b - a == 2 then True
        else False
    else False
alternative:
isPrime a && isPrime b && b - a == 2

Otherwise a pretty good solution!