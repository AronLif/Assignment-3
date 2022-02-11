import Table
import Data.Char
import PriorityQueue
import BinomialHeap

testString = "Hello my name is Aron"

testTable = Table.empty


{- characterCounts s
   RETURNS: a table that maps each character that occurs in s to the number of
         times the character occurs in s
   EXAMPLES:
 -}
-- characterCounts :: String -> Table Char Int
-- characterCounts = undefined 



---------------------------------------------------------------------------
{-        countAux letter (x:xs)
          takes a String and a Char returns the times that Char is in the String
PRE:      True
RETURNS:  Int corresponding to the times the Char is in the String
Examples: 'H' "Hej hallå" = 2
-}
countAux :: Char -> String -> Int
countAux _ [] = 0
countAux letter (x:xs)
    | letter == x = 1 + countAux letter xs
    | otherwise = countAux letter xs

{-        tableMapAux (x:xs) table
          Calls on a helper function and creates a table with keys for each Char in the String where the value is 
          How manytimes the Char is in the String
PRE:      True
RETURNS:  A table with keys for each Char in the String and corresponding values for how often the Chars are in the text
Examples: tableMapAux "test" Table.empty = T [('t',2),('e',1),('s',1)]
-}
tableMapAux :: String -> Table Char Int -> Table Char Int
tableMapAux [] table = table
tableMapAux (x:xs) table = tableMapAux (filter (/=x) xs) (Table.insert table  x (countAux (toLower x) (map toLower (x:xs)))) 

{-        characterCounts string
          Takes a String and calls a helperfunction to return a table of keys with the Charatchers in the String and Values for how many
          times they appear in the String
PRE:      True
RETURNS:  A table of keys for each Char in the String and values for how manytimes each Char is in the String
Examples: characterCounts "Hej hala"T [('H',2),('e',1),('j',1),(' ',1),('h',1),('a',2),('l',1)]
-}
characterCounts :: String -> Table Char Int
characterCounts string = tableMapAux string charTable
    where charTable = Table.empty


tabelToQue t = Table.iterate t PriorityQueue.insert PriorityQueue.empty