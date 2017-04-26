-- The sum of the squares of the first ten natural numbers is,
-- 1^2 + 2^2 + ... + 10^2 = 385
-- 
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)^2 = 55^2 = 3025
-- 
-- Hence the difference between the sum of the squares of the first ten natural
-- numbers and the square of the sum is 3025 - 385 = 2640.
-- 
-- Find the difference between the sum of the squares of the first one hundred
-- natural numbers and the square of the sum.

main = do
    (putStr . show) problem6'
    -- 25164150.

problem6 =
  let ns = [1..100]
   in square (sum ns) - sum (squares ns)
  where square n = n*n
        squares = map square

-- From greater minds than mine:
problem6' = (sum [1..100])^2 - sum (map (^2) [1..100])
