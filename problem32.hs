-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
-- through 5 pandigital.
-- 
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
-- multiplicand, multiplier, and product is 1 through 9 pandigital.
-- 
-- Find the sum of all products whose multiplicand/multiplier/product identity
-- can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to only
-- include it once in your sum.

import Data.List (elemIndices)

main = do print $ problem32

-- This is based on http://theburningmonk.com/2010/09/project-euler-problem-32-solution/.
-- He gives a justification of only checking [1000..9999], but I don't follow
-- it.
isPandigital :: Int -> Bool
isPandigital n =
  let upperBound = truncate $ sqrt $ fromIntegral n
      divisors   = filter divides [2..upperBound]
      identities = map identity divisors
   in any pandigital identities
  where divides x = n `mod` x == 0
        identity x = show x ++ show (n `div` x) ++ show n
        pandigital x = all (\d->d `elem` x && unique x d) ['1'..'9']
        unique x d = (length $ d `elemIndices` x) == 1

problem32 = sum $ filter isPandigital [1000..9999]
-- 45228, and pretty quick!

-- The only solution on the Haskell wiki involves Monads and a lot of hand-
-- waving. Meh.
