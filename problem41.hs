-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
-- For example, 2143 is a 4-digit pandigital and is also prime.
-- 
-- What is the largest n-digit pandigital prime that exists?

import Data.Char (intToDigit)
import Data.List (delete,permutations,sort)
import Primes (isPrime,primes)

main = do print $ problem41''

pandigital = (['1'..'9'] ==) . (sort . show)

problem41 = maximum $ filter pandigital $ takeWhile (<987654321) primes
-- Slow. Ran for hours. Killed it.
problem41'' = maximum $ filter pandigital' $ takeWhile (<=987654321) primes
pandigital' = (`elem` pandigitals) . show
pandigitals = permutations "0123456789"
-- Still pretty slow. Ran for hours. Killed it.

-- From greater minds than mine:
problem41' = maximum [ n' | d <- [3..9], n <- permute ['1'..intToDigit d], let n' = read n, isPrime n']
  where permute "" = [""]
        permute s  = [(x:xs)| x<-s, xs<-permute (delete x s)]
-- 7652413. And fairly quick.
