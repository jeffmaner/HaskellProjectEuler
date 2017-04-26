-- If the numbers 1 to 5 are written out in words: one, two, three, four, five,
-- then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- 
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?
-- 
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
-- letters. The use of "and" when writing out numbers is in compliance with
-- British usage.

import Data.Char (digitToInt) -- For decompose.
import Data.List (delete)
import NumbersToWords (convert)

main = do putStr . show $ problem17''

problem17' = sum $ map (sum . map length) $ map words $ map (delete '-') $ map convert [1..1000]
-- 21124.

problem17 =
  let ss = map convert [1..1000]     -- strings.
      ss' = map (delete '-') ss      -- strings without hyphens.
      ws = map words ss'             -- words.
      ls = map (sum . map length) ws -- lengths.
   in sum ls
-- 21124.

-- An interesting solution; but I think I like mine better.

one = ["one","two","three","four","five","six","seven","eight","nine",
       "ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen",
       "seventeen","eighteen","nineteen"]
ty  = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

decompose x
  | x == 0                     = []
  | x < 20                     = one !! (x-1)
  | x >= 20 && x < 100         = ty !! (firstDigit x - 2) ++ decompose (x - firstDigit x * 10)
  | x < 1000 && x `mod` 100==0 = one !! (firstDigit x - 1) ++ "hundred"
  | x > 100 && x <= 999        = one !! (firstDigit x - 1) ++ "hundredand" ++ decompose (x - firstDigit x * 100)
  | x == 1000                  = "onethousand"
  where firstDigit n = digitToInt . head . show $ n

problem17'' = length . concatMap decompose $ [1..1000]
-- 21124.
