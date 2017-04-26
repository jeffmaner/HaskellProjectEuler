-- A perfect number is a number for which the sum of its proper divisors is
-- exactly equal to the number. For example, the sum of the proper divisors of
-- 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
-- 
-- A number n is called deficient if the sum of its proper divisors is less than
-- n and it is called abundant if this sum exceeds n.
-- 
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
-- number that can be written as the sum of two abundant numbers is 24. By
-- mathematical analysis, it can be shown that all integers greater than 28123
-- can be written as the sum of two abundant numbers. However, this upper limit
-- cannot be reduced any further by analysis even though it is known that the
-- greatest number that cannot be expressed as the sum of two abundant numbers
-- is less than this limit.
-- 
-- Find the sum of all the positive integers which cannot be written as the sum
-- of two abundant numbers.

--import Data.Array
import Data.List (nub)
import Divisors (divisors)

data Classification = Deficient | Perfect | Abundant deriving (Eq,Show)

--main = print $ classify 28
--main = print $ classify 12
--main = print $ classify 28123
--main = print $ abundants'
--main = print $ divisors 28
--main = print $ isAbundant 28
--main = print $ isAbundant 12
--main = print $ abundants
--main = print $ sums
--main = print $ problem23
main = print $ problem23'

classify n
  | s < n     = Deficient
  | s > n     = Abundant
  | otherwise = Perfect
  where
    pdivs = filter (\x->n `mod` x == 0) [1..(n-1)]
    s = sum pdivs

abundants' = filter isAbundant [1..28123]
  where isAbundant n = Abundant==classify n

-- Holy crap! Look at this;
-- http://www.research.att.com/~njas/sequences/A048242
--n = 28124
--abundant n = eulerTotient n - n > n -- Hah! eulerTotient? Where is this defined?
--abunds_array = listArray (1,n) $ map abundant [1..n]
--abunds = filter (abunds_array !) [1..n]
--
--rests x = map (x-) $ takeWhile (<= x `div` 2) abunds
--isSum = any (abunds_array !) . rests
--
--problem23 = sum . filter (not . isSum) $ [1..n]

-- From theburningmonk's F# solution.
limit = 28123

isAbundant n = (sum $ divisors n) > n

abundants = filter isAbundant [1..limit]

sums = nub $ filter (<=limit) [ n+m | n<-abundants, m<-abundants ]

problem23 = (sum [1..limit]) - (sum sums)
-- I like this solution. I can understand everything it's doing.
-- 4179871.

-- From fsharp-euler's F# solution.
problem23' = sum $ filter (not . isSum) [1..limit]
  where
    limit = 28123
    isSum x = any (\a->inAbunds (x-a)) abunds
    abunds = filter isAbundant [1..limit]
    inAbunds x = x `elem` abunds
    isAbundant x = x < factorSum x
    factorSum x = sum $ filter (\i->x `mod` i == 0) [1..x `div` 2]
-- 4179871. Started last, finished first. Uh oh, my notes in J say 48511225 is
-- the answer. Hmm. My F# notes say 4179871 is the answer.

-- Why are these solutions so slow when F# is so blazingly fast? fsharp-euler's
-- solution uses arrays and binary searches. That could be part of it. But
-- theburningmonk's solution doesn't, and it's still pretty fast.
