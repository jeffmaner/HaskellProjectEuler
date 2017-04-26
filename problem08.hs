-- Find the greatest product of five consecutive digits in the 1000-digit
-- number (in problem08.txt).

-- From greater minds than mine:
import Data.Char (digitToInt)
import Data.List (tails)

main = do s <- readFile "problem08.txt"
          let ns = map digitToInt (concat $ lines s)
           in print $ maximum $ map (product . take 5) (tails ns)
           -- 40824.
