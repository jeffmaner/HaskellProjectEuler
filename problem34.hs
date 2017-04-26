-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
-- 
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
-- 
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import Data.Char (digitToInt)

main = do print $ problem34

factorial 1 = 1
factorial 2 = 2
factorial n = product [1..n]

limit = 2540160 -- 2540160 is 9!*7. 9!*8 also results in a seven-digit number, so 9!*7 is an upper limit. This is per http://www.mathblog.dk/project-euler-34-factorial-digits/.

problem34 = sum $ filter isCurious [3..limit]
  where isCurious n = (sum $ map (factorial . digitToInt) $ show n) == n
-- 40730.
