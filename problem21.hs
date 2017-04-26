-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n
-- which divide evenly into n). If d(a) = b and d(b) = a, where a /= b, then a
-- and b are an amicable pair and each of a and b are called amicable numbers.
-- 
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
-- 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
-- 71 and 142; so d(284) = 220.
-- 
-- Evaluate the sum of all the amicable numbers under 10000.

import Data.Array
import Data.List (nub, group)

--main = do print $ properDivisors 220
--main = do print $ areAmicable (220,284)
--main = do print $ areAmicable (220,285)
--main = do print problem21
main = do putStr . show $ problem21''

--d = sum . properDivisors
-- So I can define it below.

properDivisors n = filter divides [1..(n-1)]
  where divides x = (n `mod` x) == 0

areAmicable (a,b) = d a == b && d b == a && a /= b

--problem21 = filter areAmicable [(a,b) | a<-[1..300], b<-[1..300]]
--problem21 = (\t->fst t ++ snd t) $ unzip $ filter areAmicable [(a,b) | a<-[1..300], b<-[1..300]]
--problem21 = nub $ (\t->fst t ++ snd t) $ unzip $ filter areAmicable [(a,b) | a<-[1..300], b<-[1..300]]
problem21 = sum $ nub $ (\t->fst t ++ snd t) $ unzip $ filter areAmicable [(a,b) | a<-[1..9999], b<-[1..9999]]
-- I'm afraid to run this! It'll take forever, I know it.

-- So, from greater minds than mine:
-- This solution cites http://www.research.att.com/~njas/sequences/A063990,
-- which won't load for me at the moment.
problem21' = sum [m+n | m<-[2..9999], let n = divisorsSum ! m, amicable m n]
  where amicable m n = m < n && n < 10000 && divisorsSum ! n == m
        divisorsSum = array (1,9999)
                      [(i, sum (divisors i)) | i<-[1..9999]]
        divisors n = [ j | j<-[1..n `div` 2], n `mod` j == 0]
-- 31626.

-- Here is an alternative using a faster way of computing the sum of divisors.
problem21'' = sum [ n | n <- [2..9999], let m = d n,
                        m>1, m<10000, n==d m, d m /= d (d m)]

d n = product [(p * product g - 1) `div` (p-1) | g<-group $ primeFactors n, let p = head g] - n

primeFactors = pf primes
  where
    pf ps@(p:ps') n
      | p * p > n = [n]
      | r == 0    = p : pf ps q
      | otherwise = pf ps' n
      where (q,r) = n `divMod` p

primes = 2 : filter (null . tail . primeFactors) [3,5..]
-- 31626.
