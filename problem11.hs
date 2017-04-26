-- What is the greatest product of four adjacent numbers in the same direction
-- (up, down, left, right, or diagonally) in the 20×20 grid (problem11.txt)?

import Control.Arrow
import Data.Array

-- I have no idea.
-- From greater minds than mine (holy crap!):
main = print . maximum . prods . input =<< getContents

input :: String -> Array (Int,Int) Int
input = listArray ((1,1),(20,20)) . map read . words

senses = [(+1) *** id, (+1) *** (+1), id *** (+1), (+1) *** (\n -> n-1)]

inArray a i = inRange (bounds a) i

prods :: Array (Int, Int) Int -> [Int]
prods a = [product xs | i <- range $ bounds a,
                        s <- senses,
                        let is = take 4 $ iterate s i,
                        all (inArray a) is,
                        let xs = map (a!) is]
-- I have no idea what this is doing.
-- Pretty slow...
-- Okay, it's taken forever. I'll probably kill it.
