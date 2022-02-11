-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, encode, compress, decompress) where

import Table
import PriorityQueue

import Data.Char

import Test.HUnit

{- a bit code (of a character or string) is represented by a list of Booleans
   INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some Huffman tree
 -}
type BitCode = [Bool]

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------
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
tableMapAux (x:xs) table = tableMapAux (filter (/=x) xs) (Table.insert table  x (countAux (x) (x:xs))) 

{-        characterCounts string
          Takes a String and calls a helperfunction to return a table of keys with the charatchers 
          in the String and Values for how many times they appear in the String
PRE:      True
RETURNS:  A table of keys for each Char in the String and values for how manytimes each Char is in the String
Examples: characterCounts "Hej hala"T [('H',2),('e',1),('j',1),(' ',1),('h',1),('a',2),('l',1)]
-}
characterCounts :: String -> Table Char Int
characterCounts string = tableMapAux string charTable
    where charTable = Table.empty
-----------------------------------------------------------------------------------------------------------------------------------------

-- modify and add comments as needed
data HuffmanTree = Void 
  | Leaf Char Int 
  | Node Int HuffmanTree HuffmanTree deriving (Show)

tableToQueueAux :: [(Char, Int)] -> PriorityQueue HuffmanTree -> PriorityQueue HuffmanTree
tableToQueueAux [] queue = queue -- när listan är tom returnerar vi kön som vi skapat vilket är en priority Que
tableToQueueAux ((char,count):xs) queue = tableToQueueAux xs (PriorityQueue.insert queue (Leaf char count, count))

-- ta emot en lista
tableToQueue :: Table Char Int -> PriorityQueue HuffmanTree
tableToQueue table = tableToQueueAux (findChar table) PriorityQueue.empty  
  where findChar table = Table.iterate table (\s c -> c:s) []


queueToHuffman :: PriorityQueue HuffmanTree -> HuffmanTree
queueToHuffman priorityQueue = if PriorityQueue.is_empty (snd(PriorityQueue.least(priorityQueue)))
  then (fst(fst(PriorityQueue.least priorityQueue)))
  else queueToHuffman (PriorityQueue.insert (snd(PriorityQueue.least(snd(PriorityQueue.least (priorityQueue))))) (Node x l r , x))
  where l = (fst(fst(PriorityQueue.least priorityQueue)))
        r = fst(fst(PriorityQueue.least (snd(PriorityQueue.least priorityQueue))))
        x = snd(fst(PriorityQueue.least (snd(PriorityQueue.least priorityQueue)))) + snd(fst(PriorityQueue.least priorityQueue))




{- huffmanTree t
   PRE:  t maps each key to a positive value
   RETURNS: a Huffman tree based on the character counts in t
   EXAMPLES:
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree table = queueToHuffman (tableToQueue(table)) 
------------------------------------------------------------------------------------------------------------------------------------------------
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx--
------------------------------------------------------------------------------------------------------------------------------------------------

codeTableAux 


{- codeTable h
   RETURNS: a table that maps each character in h to its Huffman code
   EXAMPLES:
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable table = undefined


{- encode h s
   PRE: All characters in s appear in h
   RETURNS: the concatenation of the characters of s encoded using the Huffman code table of h.
   EXAMPLES:
 -}
encode :: HuffmanTree -> String -> BitCode
encode = undefined

{- compress s
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES:
 -} 
compress :: String -> (HuffmanTree, BitCode)
compress = undefined


{- decompress h bits
   PRE:  bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLES:
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress = undefined


--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------
{-
-- characterCounts
test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))

-- decompress
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]

-}
