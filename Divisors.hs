module Divisors (divisors) where

import Data.List (nub)

-- I based this on http://theburningmonk.com/2010/09/project-euler-problem-23-solution/.
divisors :: Int -> [Int]
divisors n =
  let uBound = truncate $ sqrt $ fromIntegral n
      divs   = filter (\x->n `mod` x == 0) [1..uBound]
      divs'  = concatMap (\x->[x, n `div` x]) divs
   in nub $ filter (/=n) divs'
