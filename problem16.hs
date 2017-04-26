-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
-- 
-- What is the sum of the digits of the number 2^1000?

import Data.Char (digitToInt)

--main = do putStr . show . sum . map digitToInt . show $ 2^1000
-- 1366.
main = do putStr . show $ problem16

problem16' = sum . map digitToInt . show $ 2^1000

-- Hmm. Here's someone else's solution:
problem16 = sum k
  where s = show (2^1000)
        k = map digitToInt s
