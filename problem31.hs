-- In England the currency is made up of pound, £, and pence, p, and there are
-- eight coins in general circulation:
-- 
--     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- 
-- It is possible to make £2 in the following way:
-- 
--     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
-- 
-- How many different ways can £2 be made using any number of coins?

-- I actually did some research on this problem when working on F#, but I don't
-- feel like doing it again. So I'll just copy from my F# solution.

main = do print $ problem31'

total = 200
coins = [1,2,5,10,20,50,100,200]

count :: Int -> Int -> Int
count n m
  | n==0         = 1
  | n< 0         = 0
  | m<=0 && n>=1 = 0
  | otherwise    = (count n (pred m)) + count (n-coins!!pred m) m

problem31 = count total $ length coins
-- 73682.

-- Wow! Lots of solutions on the Haskell wiki. :)
-- I'll just go with the last one.

countCoins 1 _ = 1
countCoins n x = sum $ map addCoin [0 .. x `div` coins !! pred n]
  where addCoin k = countCoins (pred n) (x - k * coins !! pred n)

problem31' = countCoins (length coins) 200
-- 73682.
