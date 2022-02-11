-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, encode, compress, decompress) where

import Table
import PriorityQueue
import Data.Maybe
import Test.HUnit
import qualified BinomialHeap

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
tableMapAux (x:xs) table = tableMapAux (filter (/=x) xs) (Table.insert table  x (countAux x (x:xs)))

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


-- modify and add comments as needed
data HuffmanTree = Leaf (Char, Int) 
  | Node HuffmanTree Int HuffmanTree deriving(Show,Eq)

{-        priorityqueue t
PRE:      True
RETURNS:  A priorityqueue HuffmanTree based on the table recived
Examples: priorityqueue $ characterCounts "foo" = [Node 1 1 (Leaf ('f',1)) [Node 0 2 (Leaf ('o',2)) []]]
-}
priorityqueue :: Table Char Int -> PriorityQueue HuffmanTree
priorityqueue t = Table.iterate t (\pq (k,v) -> PriorityQueue.insert pq (Leaf (k, v), v)) PriorityQueue.empty

{-        huffmanTreeAux t
PRE:      True
RETURNS:  a HuffmanTree based on the priorityqueue Huffmantree that's recived
Examples: huffmanTreeAux $ priorityqueue $ characterCounts "foo" = Node (Leaf ('f',1)) 3 (Leaf ('o',2))
-}
huffmanTreeAux :: PriorityQueue HuffmanTree -> HuffmanTree
huffmanTreeAux t
  |PriorityQueue.is_empty (snd (PriorityQueue.least t)) = fst (fst(PriorityQueue.least t)) 
  |otherwise= huffmanTreeAux (PriorityQueue.insert (snd (PriorityQueue.least (snd(PriorityQueue.least t)))) 
    (mergetree (fst(fst(PriorityQueue.least t))) (fst(fst(PriorityQueue.least (snd (PriorityQueue.least t))))), 
    snd(fst(PriorityQueue.least t)) + snd(fst (PriorityQueue.least(snd(PriorityQueue.least t))))))          

{-        mergetree (Leaf (a,b)) (Leaf (c,d))
PRE:      True
RETURNS:  a Node with the values of the leaf and or Nodes recived where the Int in the new Node is the 
          Int values added together from the recived data 
Examples: mergetree (Node (Leaf ('f',1)) 3 (Leaf ('o',2))) (Leaf ('a',1)) = Node (Node (Leaf ('f',1)) 3 (Leaf ('o',2))) 4 (Leaf ('a',1))
-}
mergetree :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergetree (Leaf (a,b)) (Leaf (c,d)) = Node (Leaf (a,b)) (b+d) (Leaf (c,d))
mergetree (Leaf (a,b)) (Node left n right) = Node (Leaf (a,b)) (b+n) (Node left n right)
mergetree (Node left n right) (Leaf (a, b)) = Node (Node left n right) (n+b) (Leaf (a, b))
mergetree (Node a b c) (Node d e f) = Node (Node a b c) (b + e) (Node d e f) 

{-        huffmanTree t
PRE:      True
RETURNS:  A HuffmanTree based on the recived Table 
Examples: huffmanTree $ characterCounts "foo" = Node (Leaf ('f',1)) 3 (Leaf ('o',2))
-}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t 
  | PriorityQueue.is_empty (priorityqueue t) = Leaf (' ', 0)
  | otherwise = huffmanTreeAux (priorityqueue t)


-- Kommentera gärna detta

delete1 :: [(Char,BitCode)] -> [(Char, BitCode)]
delete1 (x:xs) = [a | a <- xs, a /= x]


insert1 :: [(Char,BitCode)] -> Table Char BitCode -> Table Char BitCode
insert1 [] t = t
insert1 s t1 = insert1 (delete1 s) (uncurry (Table.insert t1) (head s))



codeTableAux :: HuffmanTree -> [(Char, BitCode)]
codeTableAux (Leaf (c, i)) = [(c, [])]
codeTableAux (Node left n right) = map (add False) (codeTableAux left) ++ map (add True) (codeTableAux right)
  where add = \a (c, i) -> (c,a:i)
------------------------------------------------------------------------------------------------------------------





{- codeTable h
   RETURNS: a table that maps each character in h to its Huffman code
   EXAMPLES: codeTable $Node (Leaf ('f',1)) 3 (Leaf ('o',2)) = T [('f',[False]),('o',[True])]
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable t = insert1 (codeTableAux t) Table.empty

{- encode h s
   PRE: All characters in s appear in h
   RETURNS: the concatenation of the characters of s encoded using the Huffman code table of h.
   EXAMPLES: encode (Node (Leaf ('f',1)) 3 (Leaf ('o',2))) "foo" = [False,True,True]
 -}
encode :: HuffmanTree -> String -> BitCode
encode hufftree string = encodeAux string (codeTable hufftree)

{-        encodeAux
PRE:      True
RETURNS:  a BitCode based on the string and table recived
Examples: encodeAux "foo" $codeTable $ Node (Leaf ('f',1)) 3 (Leaf ('o',2)) = [False,True,True]
-}
encodeAux :: String -> Table Char BitCode -> BitCode
encodeAux [] _ = []
encodeAux (x:xs) table = charInTable x table ++ encodeAux xs table

--------------------------------------------------------------------------------------------------------------------
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx--
--------------------------------------------------------------------------------------------------------------------
{-        charInTable char table
PRE:      True
RETURNS:  if there is
Examples: 
-}
charInTable :: Char -> Table Char BitCode -> BitCode
charInTable char table = btLookUp table char
  where btLookUp tale char = Data.Maybe.fromMaybe [] (Table.lookup table char)
-------------------------------------------------------------------------------------------
---  encode 
-------------------------------------------------------------------------------------------


{- compress s
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES:
 -}
compress :: String -> (HuffmanTree, BitCode)
compress string = (hfTree, encode hfTree string)
  where hfTree = huffmanTree $ characterCounts string


{- decompress h bits
   PRE:  bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLES:
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress (Leaf (char,int)) [] = singlechar char int
decompress _ [] = []
decompress huff bit = fst(translateLetter bit huff) ++ decompress huff (snd(translateLetter bit huff)) 

translateLetter :: [Bool] -> HuffmanTree -> ([Char], [Bool])
translateLetter [] (Leaf (a, _)) = ([a], [])
translateLetter [] _ = ([],[]) 
translateLetter (x:xs) (Leaf (a,b)) = ([a], x:xs)
translateLetter (x:xs) (Node l int r) 
  | x = translateLetter xs r
  | otherwise = translateLetter xs l


singlechar char 0 = []
singlechar char int = [char] ++ singlechar char (int -1)









--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------

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
