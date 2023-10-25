module TextFrame where
{- 
   Inspiration from the f-string in Python 3.6. Using the printf function from Text.Printf.
   After having the basic structure it was need to declare a auxiliar variable to store the
   separator. In this case I found the let ... in ... expression very useful.
-}
import Text.Printf

textInFrame :: String -> Char -> String
textInFrame main_str char2separate =
   let separator = replicate (length main_str + 4) char2separate
   in printf "%s\n%c %s %c\n%s\n" separator char2separate main_str char2separate separator