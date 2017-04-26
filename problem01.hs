-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we
-- get 3, 5, 6 and 9. The sum of these multiples is 23.
-- 
-- Find the sum of all the multiples of 3 or 5 below 1000.

import Data.List (union)

main = do
    (putStr . show) $ problem1
    -- 233168.

problem1 = sum [ x | x<-[1..999], (x `mod` 3)==0 || (x `mod` 5)==0]

-- From greater minds than mine:
problem1' = sum (union [3,6..999] [5,10..999])

problem1'' = sumStep 3 999 + sumStep 5 999 - sumStep 15 999
  where
    sumStep s n = s * sumOneToN (n `div` s)
    sumOneToN n = n * (n+1) `div` 2
    -- div returns how many times the first number can be divided by the second one.
    -- 6 `div` 2 == 3
    -- 8 `div` 3 == 2
