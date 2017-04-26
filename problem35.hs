-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves
-- prime.
-- 
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
-- 
-- How many circular primes are there below one million?

import Control.Monad (replicateM)
import Data.List (permutations)
import Primes (primes,isPrime)

main = do print $ problem35''

primes' = takeWhile (< 1000000) primes

isCircular = foldr1 (&&) . map ((`elem` primes') . read) . permutations . show

problem35 = length $ filter isCircular $ primes'
-- 22. The answer is 55. :(

-- Let's see what greater minds than mind came up with.

circularPrimes :: [Integer] -> [Integer]
circularPrimes [] = []
circularPrimes (x:xs)
  | all isPrime ps = x : circularPrimes xs
  | otherwise      = circularPrimes xs
  where ps = map read $ permutations $ show x

problem35' = length $ circularPrimes $ takeWhile (<1000000) primes
-- 22. Huh? The answer is 55, isn't it?

-- ... observing that one can greatly reduce the search space because no circular prime can contain an even number,
-- nor a 5,...
canBeCircularPrimeList = [1,3,7,9]

listToInt n = foldl (\x y->10*x+y) 0 n
rot n l = y++x where (x,y) = splitAt n l
allRots l = map (\x->rot x l) [0..(length l)-1]
isCircular' l = all (isPrime . listToInt) $ allRots l
circular 1 = [[2],[3],[5],[7]] -- A slightly special case.
circular n = filter isCircular' $ replicateM n canBeCircularPrimeList

problem35'' = length $ concatMap circular [1..6]
-- 55, and blindingly fast!
