-- If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly
-- three solutions for p = 120.
-- 
-- {20,48,52}, {24,45,51}, {30,40,50}
-- 
-- For which value of p <= 1000, is the number of solutions maximised?

import Data.List (elemIndex,group,maximumBy,sort)
import Data.Ord (comparing)

main = do print $ problem39'

problem39 = maximumBy (comparing length) $ group $ sort [x+y+z|x<-[1..1000], y<-[1..(1000-x)], z<-[1..(1000-x-y)], x^2 + y^2 == z^2]
-- [840,840,840,840,840,840,840,840,840,840,840,840,840,840,840,840]. I almost did it! It ran for a while, but I
-- almost did it all by myself! :)

-- From greater minds than mine:
-- We use the well known formula to generate primitive Pythagorean triples. All we need are the perimeters, and
-- they have to be scaled to produce all triples in the problem space.
problem39' = head $ perims !! indexMax
  where perims = group $ sort [n*p | p <- pTriples, n <- [1..1000 `div` p]]
        counts = map length perims
        Just indexMax = elemIndex (maximum counts) $ counts
        pTriples = [p |
                    n <- [1..floor (sqrt 1000)],
                    m <- [n+1..floor (sqrt 1000)],
                    even n || even m,
                    gcd n m == 1,
                    let a = m^2 - n^2,
                    let b = 2*m*n,
                    let c = m^2 + n^2,
                    let p = a+b+c,
                    p<1000]
-- 840.
