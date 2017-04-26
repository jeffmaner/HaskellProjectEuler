-- A unit fraction contains 1 in the numerator. The decimal representation of
-- the unit fractions with denominators 2 to 10 are given:
-- 
--     1/2	= 	0.5
--     1/3	= 	0.(3)
--     1/4	= 	0.25
--     1/5	= 	0.2
--     1/6	= 	0.1(6)
--     1/7	= 	0.(142857)
--     1/8	= 	0.125
--     1/9	= 	0.(1)
--     1/10	= 	0.1
-- 
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
-- seen that 1/7 has a 6-digit recurring cycle.
-- 
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle
-- in its decimal fraction part.

-- From greater minds than mine:
import Data.List (maximumBy, elemIndex)
import Data.Ord (comparing)

main = do print $ problem26

problem26 = fst $ maximumBy (comparing snd) [(n, recurringCycle n) | n<-[1..999]]
  where
    recurringCycle d = remainders d 10 []
    remainders d 0 rs = 0
    remainders d r rs = let r' = r `mod` d
                         in case elemIndex r' rs of
                              Just i  -> i+1
                              Nothing -> remainders d (10*r') (r':rs) 
-- 983.
-- I cannot make any sense of this solution. But it works.
