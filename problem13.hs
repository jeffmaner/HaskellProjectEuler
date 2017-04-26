-- Work out the first ten digits of the sum of the one-hundred 50-digit numbers
-- in problem13.txt.

main = do
  contents <- readFile "problem13.txt"
  (putStr . take 10 . show . sum . map read . lines) contents
  -- 5537376230.
  -- Woo-hoo! I did it myself! :)

-- Hmm. From greater minds than mine:
main = do xs <- fmap (map read . lines) (readFile "problem13.txt")
          print . take 10 . show . sum $ xs
