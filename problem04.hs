-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
-- 
-- Find the largest palindrome made from the product of two 3-digit numbers.

main = do
    (putStr . show) problem4
    -- 906609.

problem4 = maximum [x | y<-[100..999], z<-[100..999], let x=y*z, let s=show x, s == reverse s]
-- This is basically my solution, cleaned up a little with stuff I didn't
-- think about.
-- List comprehensions are amazing!
