-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- 
-- Find the sum of all the primes below two million.

import Primes (primes)

main = do
  (putStr . show) problem10'

problem10 = sum [n|n<-primes, n<2000000]
-- Dreadfully slow. Killed it.

-- From greater minds than mine:
problem10' = sum $ takeWhile (< 2000000) primes
-- Basically the same as mine...
-- And pretty slow, but faster than mine. Huh.
-- 142913828922.
