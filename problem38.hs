-- Take the number 192 and multiply it by each of 1, 2, and 3:
-- 
--     192 × 1 = 192
--     192 × 2 = 384
--     192 × 3 = 576
-- 
-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the
-- concatenated product of 192 and (1,2,3)
-- 
-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital,
-- 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
-- 
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of
-- an integer with (1,2, ... , n) where n > 1?

import Data.List (sort)

main = do print $ problem38

-- isPandigital yanked from problem32.
--isPandigital :: Int -> Bool
--isPandigital n =
--  let upperBound = truncate $ sqrt $ fromIntegral n
--      divisors   = filter divides [2..upperBound]
--      identities = map identity divisors
--   in any pandigital identities
--  where divides x = n `mod` x == 0
--        identity x = show x ++ show (n `div` x) ++ show n
--        pandigital x = all (\d->d `elem` x && unique x d) ['1'..'9']
--        unique x d = (length $ d `elemIndices` x) == 1

--problem38 = maximum $ [x|x<-[1..987654321], isPandigital x]
-- I have no idea how to do this.

-- Hmm...
mult n i vs
  | length (concat vs) >= 9 = concat vs
  | otherwise               = mult n (i+1) (vs ++ [show (n*i)])
-- I'm not entirely clear on what this is doing...

problem38 :: Int
problem38 = maximum . map read . filter ((['1'..'9'] ==) . sort) $ [mult n 1 [] | n <- [2..9999]]
-- 932718654.
-- Interesting solution for isPandigital: ((['1'..'9'] ==) . sort).
-- 9999 seems arbitrary.
