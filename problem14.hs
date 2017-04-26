-- The following iterative sequence is defined for the set of positive integers:
-- 
-- n -> n/2 (n is even)
-- n -> 3n + 1 (n is odd)
-- 
-- Using the rule above and starting with 13, we generate the following
-- sequence:
-- 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
-- 
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz Problem), it
-- is thought that all starting numbers finish at 1.
-- 
-- Which starting number, under one million, produces the longest chain?
-- 
-- NOTE: Once the chain starts the terms are allowed to go above one million.

collatz :: Int -> Int
collatz n
  | even n = n `div` 2
  | odd  n = 3*n+1

cSequence n = f n [n]
  where f m xs
          | m==1 = reverse xs
          | otherwise = let c = collatz m
                         in f c (c:xs)

--longest lists =
--  let ts = lengths lists
--      m = maximum $ map fst ts
--   in snd $ head $ filter (\(n,xs)->n==m) ts
--  where lengths xss = map (\xs->(length xs, xs)) xss

--main = do putStr . unlines . map (unwords . map show) $ map cSequence [1..13]
--main = do putStr . unlines . map (\(n,xs)->(show n)++": "++(unwords $ map show xs)) . lengths $ map cSequence [1..13]
--main = do putStr . show . head . longest $ map cSequence [1..13]
--main = do putStr . show . head . longest $ map cSequence [1..999999]
-- Out of memory! :)
--
-- I'm not going to implement any of the three from the greater minds than mine
-- because I don't understand them fully. But, one of them used maximumBy, which
-- I should try next time I need longest. Looks like it's something like
-- maximumBy (compare `on` length) xs.

lengthAndHead ys@(x:xs) = (length ys, x)

longest ts =
  let m = maximum $ map fst ts
   in snd $ head $ filter (\(n,_)->n==m) ts

main = do putStr . show $ longest $ map (lengthAndHead . cSequence) [1..999999]
-- Out of memory.
