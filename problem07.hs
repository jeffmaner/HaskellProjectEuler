-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
-- that the 6th prime is 13.
-- 
-- What is the 10,001st prime number?

import Primes (primes)

main = do
    (putStr . show) problem7
    -- 104743.

-- From minds greater than mine:
problem7 = primes !! 10000
