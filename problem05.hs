-- 2520 is the smallest number that can be divided by each of the numbers from
-- 1 to 10 without any remainder.
-- 
-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?

main = do
    (putStr . show) problem5'
    -- 232792560.

problem5 = minimum [y|x<-[1..20], y<-[1..factorial 20], y `mod` x == 0]
-- 1. LOL, obviously, I'm missing something.

factorial n = product [1..n]

-- From greater minds than mine.

problem5' = foldr1 lcm [1..20]
-- ! I love Haskell!
