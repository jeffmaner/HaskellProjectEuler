-- Starting in the top left corner of a 2×2 grid, and only being able to move
-- to the right and down, there are exactly 6 routes to the bottom right corner.
-- 
-- How many such routes are there through a 20×20 grid?

main = do putStr . show $ problem15'

-- This is beyond my ken, so from greater minds than mine:
problem15 = iterate (scanl1 (+)) (repeat 1) !! 20 !! 20
-- 137846528820.

problem15' = product [21..40] `div` product [2..20]
-- 137846528820.
