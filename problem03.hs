-- The prime factors of 13195 are 5, 7, 13 and 29.
-- 
-- What is the largest prime factor of the number 600851475143 ?

import Primes (primeFactors)

main = do
    (putStr . show) problem3
    -- 6857.

-- From greater minds than mine.
problem3 = last $ primeFactors 600851475143

-- Another solution, not using recursion:
problem3' = (m!!0) `div` (m!!1)
  where
    m = reverse $ takeWhile (<=n) (scanl1 (*) [x|x<-2:[3,5..], (n `mod` x)==0])
    n = 600851475143
