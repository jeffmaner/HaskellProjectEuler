-- Starting with the number 1 and moving to the right in a clockwise direction
-- a 5 by 5 spiral is formed as follows:
-- 
-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13
-- 
-- It can be verified that the sum of the numbers on the diagonals is 101.
-- 
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
-- formed in the same way?

main = do print $ problem28' 1001

-- This solution is given on the Haskell wiki, with no explanation.
problem28 = sum (map (\n -> 4*(n-2)^2+10*(n-1)) [3,5..1001]) + 1
-- This formula probably comes from something I could Google for.
-- 669171001.

-- Alternatively one can use the fact that the distance between the diagonal
-- numbers increases by 2 in every concentric square. Each square contains four
-- gaps, so the following scanl does the trick:
problem28' n = sum $ scanl (+) 0 (1:(concatMap (replicate 4) [2,4..(n-1)]))
-- 669171001.

-- Awesome discussion of an analytical approach to solving this at
-- http://www.mathblog.dk/project-euler-28-sum-diagonals-spiral/.
