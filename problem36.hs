-- The decimal number, 585 = 1001001001b2 (binary), is palindromic in both bases.
-- 
-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
-- 
-- (Please note that the palindromic number, in either base, may not include leading zeros.)

-- I have no idea how to do this!
-- My F# answers rely on .Net, or are inherently imperative, so that won't do.
-- I coded an answer based on http://www.mathblog.dk/project-euler-36-palindromic-base-10-2/ that
-- I will try to code here before checking out the Haskell wiki.

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

main = do print $ problem36''

isPalindrome b n = n == reversed n 0
  where reversed k m
          | k>0       = reversed (k `div` b) (b*m + k `mod` b)
          | otherwise = m

isPalindromeB10 = isPalindrome 10
isPalindromeB2  = isPalindrome  2

problem36 = sum $ filter (\n -> isPalindromeB10 n && isPalindromeB2 n) [1,3..1000000] -- No even can be a palindrome, because the binary cannot start with a zero.
-- 872187. Whoo-hoo!
problem36' = sum [n|n<-[1,3..1000000], isPalindromeB10 n, isPalindromeB2 n]
-- 872187. Whoo-hoo!

-- Okay, on to the Haskell wiki.
showB = flip (showIntAtBase 2 intToDigit) ""
isPalindrome' x = x == reverse x
problem36'' = sum [x | x <- [1,3..1000000], isPalindrome' (show x), isPalindrome' (showB x)]
-- 827187.
