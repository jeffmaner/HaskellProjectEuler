module NumbersToWords (convert) where

-- From chapter four of /Introduction to Functional Programming/, by Richard
-- Bird and Philip Wadler.

convert2 n = combine2 $ digits2 n
digits2 n = (n `div` 10, n `mod` 10)

units = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
         "seventeen", "eighteen", "nineteen"]
tens  = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
         "ninety"]

-- Why don't these awesome patterns work? They used to!
--combine2 (0, u+1)   = units !! u
combine2 (0, u)   = units !! (u-1)
--combine2 (1, u)     = teens !! u
combine2 (1, u)     = teens !! u
--combine2 (t+2, 0)   = tens  !! t
combine2 (t, 0)   = tens  !! (t-2)
--combine2 (t+2, u+1) = tens  !! t ++ "-" ++ units !! u
combine2 (t, u) = tens  !! (t-2) ++ "-" ++ units !! (u-1)

convert3 n = combine3 $ digits3 n
digits3 n  = (n `div` 100, n `mod` 100)

--combine3 (0, t+1)   = convert2 (t+1)
combine3 (0, t)   = convert2 t
--combine3 (h+1, 0)   = units !! h ++ " hundred"
combine3 (h, 0)   = units !! (h-1) ++ " hundred"
--combine3 (h+1, t+1) = units !! h ++ " hundred and " ++ convert2 (t+1)
combine3 (h, t) = units !! (h-1) ++ " hundred and " ++ convert2 t

convert6 n = combine6 $ digits6 n
digits6 n = (n `div` 1000, n `mod` 1000)

--combine6 (0, h+1)   = convert3 (h+1)
combine6 (0, h)   = convert3 h
--combine6 (m+1, 0)   = convert3 (m+1) ++ " thousand"
combine6 (m, 0)   = convert3 m ++ " thousand"
--combine6 (m+1, h+1) = convert3 (m+1) ++ " thousand" ++ link (h+1) ++ convert3 (h+1)
combine6 (m, h) = convert3 m ++ " thousand" ++ link h ++ convert3 h

link h
  | h < 100   = " and "
  | otherwise = " "

convert = convert6

-- main = do
--   number <- getLine
--   if null number
--      then return ()
--      else do
--           putStrLn $ convert . read number --(read number :: Int)
--           main
