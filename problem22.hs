-- Using names.txt, a 46K text file containing over five-thousand first names,
-- begin by sorting it into alphabetical order. Then working out the
-- alphabetical value for each name, multiply this value by its alphabetical
-- position in the list to obtain a name score.
-- 
-- For example, when the list is sorted into alphabetical order, COLIN, which
-- is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
-- would obtain a score of 938 × 53 = 49714.
-- 
-- What is the total of all the name scores in the file?

import Data.List (sort)
import Data.Char (ord)

--main = do print scores
--main = do print $ worth "COLIN"
main = do
  contents <- readFile "names1.txt"
  putStr . show $ problem22 contents
--main = do
--  contents <- readFile "names.txt"
--  putStr . show $ problem22' contents

--problem22 contents = sum $ map score $ zip [1..] $ sort $ words contents
--  where score (r,n) = r*worth n
-- 871198282.
-- zipWith! Good idea:
problem22 contents = sum $ zipWith score [1..] $ sort $ words contents
  where score r n = r * worth n
-- 871198282.

scores = zip ['A'..'Z'] [1..26]

worth = sum . map w
  where w c = snd $ head $ filter (\t->c==fst t) scores

-- I actually really like my solution. But here's another.
problem22' contents =
  let names = sort $ read $ "["++contents++"]"
      scores = zipWith score names [1..]
   in sum scores
  where score w i = (i *) . sum . map (\c->ord c - ord 'A' + 1) $ w
-- 871198282.
