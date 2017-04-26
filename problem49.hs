-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two
-- ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one
-- another.
-- 
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but
-- there is one other 4-digit increasing sequence.
-- 
-- What 12-digit number do you form by concatenating the three terms in this sequence?

-- The answer on the Haskell wiki uses millerRabinPrimality to test primality. I'll try without that.
import Data.List (sort)
import Primes (isPrime)

main = do print $ problem49

primes4 = filter isPrime [1488..9999]

problem49 = [ (a,b,c) | a <- primes4,
                        b <- dropWhile (<=a) primes4,
                        sort (show a) == sort (show b),
                        let c = 2*b-a, -- I wish I understood the justification of this.
                        c `elem` primes4,
                        sort (show a) == sort (show c) ]
-- [(2969,6299,9629)], and pretty quick, too.
