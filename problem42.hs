-- The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
-- 
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- 
-- By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we
-- form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle
-- number then we shall call the word a triangle word.
-- 
-- Using problem42.txt, a 16K text file containing nearly two-thousand common English words, how many are triangle words?

import Data.Char (ord)

main = do
  contents <- readFile "problem42.txt"
  let names = read $ "["++contents++"]"
  print $ length $ filter triangle names
-- 162. And fast! Yay!
  --f<-readFile "problem42.txt"
  --let words = read $ "["++f++"]"
  --print $ problem42 words

triangle name = score `elem` ts
  where score = worth name
        ts = map t [1..score]

--t n = truncate $ fromIntegral (n*(n+1))/2
t n = (n*(n+1)) `div` 2

scores = zip ['A'..'Z'] [1..26]

worth = sum . map w
  where w c = snd $ head $ filter (\t->c==fst t) scores

-- From greater minds than mine... maybe...
trilist = takeWhile (<300) (scanl1 (+) [1..]) -- Where does magic number 300 come from?
wordscore xs = sum $ map (subtract 64 . ord) xs
problem42 megalist = length [wordscore a | a<-megalist, elem (wordscore a) trilist]
-- 162, and maybe a second or two faster than mine. But mine's so much more clear!
