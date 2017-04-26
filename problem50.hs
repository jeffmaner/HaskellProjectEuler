-- The prime 41 can be written as the sum of six consecutive primes:
-- 41 = 2 + 3 + 5 + 7 + 11 + 13
-- 
-- This is the longest sum of consecutive primes that adds to a prime below one-hundred.
-- 
-- The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
-- 
-- Which prime, below one-million, can be written as the sum of the most consecutive primes?

import Data.List (inits,sort,tails)
import Primes (isPrime,primes)

main = do print $ problem50

limit = 1000000

primeCountsAndSums' =
  let ps = takeWhile (<limit) primes
      is = inits ps
      ts = tails ps
      is' = predicate $ concatMap tails is
      ts' = predicate $ concatMap inits ts
      --is'' = predicate is -- With this change, it's not running out of memory, but it is running forever.
      --ts'' = predicate ts -- Killed it after running through lunch.
   in sort $ is' ++ ts'
  where predicate = filter (isPrime . snd) . filter ((<limit) . snd) . map (\xs->(length xs, sum xs))
-- This version still runs out of memory.

primeCountsAndSums =
  let ps = takeWhile (<limit) primes
      is = inits ps
      ts = tails ps
      is' = concatMap tails is
      ts' = concatMap inits ts
   in sort $ filter (isPrime . snd) $ filter ((<limit) . snd) $ map (\xs->(length xs, sum xs)) $ is ++ ts ++ is' ++ ts'

problem50 = last $ primeCountsAndSums'
-- (21,953) for limit 1000.
-- Out of memory for limit 1e6.

-- The solutions on the Haskell wiki don't make sense to me.
