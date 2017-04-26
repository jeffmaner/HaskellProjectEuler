-- Euler discovered the remarkable quadratic formula:
-- 
-- n² + n + 41
-- 
-- It turns out that the formula will produce 40 primes for the consecutive
-- values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is
-- divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly
-- divisible by 41.
-- 
-- The incredible formula  n² - 79n + 1601 was discovered, which produces 80
-- primes for the consecutive values n = 0 to 79. The product of the
-- coefficients, -79 and 1601, is -126479.
-- 
-- Considering quadratics of the form:
-- 
--     n² + an + b, where |a| < 1000 and |b| < 1000
-- 
--     where |n| is the modulus/absolute value of n
--     e.g. |11| = 11 and |-4| = 4
-- 
-- Find the product of the coefficients, a and b, for the quadratic expression
-- that produces the maximum number of primes for consecutive values of n,
-- starting with n = 0.

-- I don't even know where to begin on this. The solution on the Haskell site is
-- good, but there's no explanation given--it's like they just pulled it out of
-- the air. So I'm not going to reproduce it here.
-- There some more comprehensible F# solutions, though.

-- From http://theburningmonk.com/2010/09/project-euler-problem-27-solution/:
-- (ran for several minutes in the IDE, so I killed it).
-- (Found a bug that may have caused it to run indefinitely...)

import Data.List (concatMap, findIndex, maximumBy)
import Data.Ord (comparing)

--main = do print $ primeCount 1 41
main = do print $ problem27'

hasDivisor n =
  let m = truncate $ sqrt $ fromIntegral n
   in any (\x->n `mod` x == 0) [2..m]

isPrime n = if n <= 1 then False else not $ hasDivisor n

f n a b = n*n + a*n + b -- The quadratic expression.

primeCount a b = length $ takeWhile (\n->isPrime (f n a b)) [1..]

aList = [-999..999]
bList = filter isPrime [2..999]

problem27 =
  --let (a,b,_) = maximumBy (comparing (\(_,_,count)->count)) $ concatMap (\a-> map (\b->(a,b, primeCount a b)) (filter (\b->a+b>=1) bList)) aList
  let (a,b,_) = maximumBy (comparing thd) $ map (\(a,b)->(a,b, primeCount a b)) [(a,b)|a<-aList, b<-bList, a+b>=1]
   in a*b
  where thd (a,b,c) = c
-- -59231. Wow!

-- Here's another based on F#:
-- From http://fsharp-euler.wikispaces.com/euler+027:
problem27' =
  maximum [((count i j), i, j)|i<-[-999..999], j<-[-999..999]]
  where isPrime n = if n==1 then False else let m = truncate $ sqrt $ fromIntegral n in all (\i->n `mod` i /= 0) [2..m]
        notPrime x = (x <= 0) || (not $ isPrime x)
        count a b = findIndex notPrime $ map (\n->n*n + a*n + b) [0..]
-- (Just 71, -61, 971). Much slower. Probably j<-[-999..999].

