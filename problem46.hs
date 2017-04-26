-- It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and
-- twice a square.
-- 
--  9 =  7 + 2×1^2
-- 15 =  7 + 2×2^2
-- 21 =  3 + 2×3^2
-- 25 =  7 + 2×3^2
-- 27 = 19 + 2×2^2
-- 33 = 31 + 2×1^2
-- 
-- It turns out that the conjecture was false.
-- 
-- What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

import Primes (isPrime,primes)

main = do print $ problem46'

-- Based on my F# solution from the burning monk.
oddCompositeNumbers = filter (not . isPrime) [9,11..]

isSum n = any (\x->sqrt (fromIntegral ((n-x) `div` 2)) == 0) $ takeWhile (<n) primes

problem46 = head $ filter (not . isSum) oddCompositeNumbers
-- 9. Um, no. :)

-- From greater minds than mine:
compOdds :: [Integer]
compOdds = filter (not . isPrime) [3,5..]

verifyConj :: Integer -> Bool
verifyConj n = any isPrime (takeWhile (>0) $ map (\i->n-2*i*i) [1..])

problem46' :: Integer
problem46' = head $ filter (not . verifyConj) compOdds
-- 5777.
