module Primes (isPrime, primes, primeFactors) where

-- From greater minds than mine.
primes :: [Integer]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
  where
    factor m (p:ps)
      | p*p > m        = [m]
      | m `mod` p == 0 = p : factor (m `div` p) (p:ps)
      | otherwise      = factor m ps

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = case primeFactors n of
              (_:_:_)   -> False
              otherwise -> True
