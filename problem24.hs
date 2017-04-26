-- A permutation is an ordered arrangement of objects. For example, 3124 is one
-- possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
-- are listed numerically or alphabetically, we call it lexicographic order. The
-- lexicographic permutations of 0, 1 and 2 are:
-- 
-- 012   021   102   120   201   210
-- 
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
-- 5, 6, 7, 8 and 9?

-- I have no idea how to start this one. So, from greater minds than mine...

import Data.List --(delete, permutations, sort)

main = do print $ problem24''

fac 0 = 1
fac n = n * fac (n-1)

perms [] _ = []
perms xs n = x : perms (delete x xs) (mod n m)
  where m = fac $ length xs - 1
        y = div n m
        x = xs!!y

problem24 = perms "0123456789" 999999
-- "2783915460".

-- Or, using Data.List (permutations):
problem24' = (!! 999999) . sort $ permutations ['0'..'9']
-- "2783915460", but slower! Weird.

-- Okay, I don't understand this solution, but...
factorial n = product [1..n]

lexOrder digits left s
  | len == 0         = s ++ digits
  | q > 0  && r == 0 = lexOrder (digits\\(show (digits!!(q-1)))) r (s ++ [(digits!!(q-1))])
  | q == 0 && r == 0 = lexOrder (digits\\(show (digits!!len)))   r (s ++ [(digits!!len)])
  | r == 0           = lexOrder (digits\\(show (digits!!(q+1)))) r (s ++ [(digits!!(q+1))])
  | otherwise        = lexOrder (digits\\(show (digits!!q)))     r (s ++ [(digits!!q)])
  where
    len = (length digits) - 1
    (q,r) = quotRem left (factorial len)

problem24'' = lexOrder "0123456798" 1000000 ""
-- "2783915460". And a little faster, maybe.
