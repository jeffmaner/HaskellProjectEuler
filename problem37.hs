-- The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from
-- left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left:
-- 3797, 379, 37, and 3.
-- 
-- Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
-- 
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

-- I didn't like any of the F# code source solutions. I found a regex solution which was pretty cool. But I think
-- I'll jump directly to the Haskell wiki to see what greater minds than mine came up with.

import Data.List (tails, inits, nub)
--import Data.Numbers.Primes (primes, isPrime) -- Holy crap! Really?! Ah. "Count not find module". Okay.
import Primes (primes, isPrime)

main = do print $ problem37'

-- Interpreter complained about Ints and Integers, so I just commented out the type signatures.
--test' :: Int -> Int -> (Int -> Int -> Int) -> Bool
test' n d f
  | d > n     = True
  | otherwise = isPrime (f n d) && test' n (10*d) f

--test :: Int -> Bool
test n = test' n 10 (mod) && test' n 10 (div)

problem37 = sum $ take 11 $ filter test $ filter (>7) primes
-- 748317.

-- Hmm. I think the other answer is decent, and I can follow it, mostly:
truncs :: Integer -> [Integer]
truncs n = nub . map read $ (take m . tail . tails) s ++ (take m . tail . inits) s
  where
    m = length s - 1
    s = show n

problem37' = sum $ take 11 [x|x<-dropWhile (<=9) primes, all isPrime (truncs x)]
-- 748317.
