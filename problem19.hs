-- You are given the following information, but you may prefer to do some
-- research for yourself.
-- 
--    * 1 Jan 1900 was a Monday.
--    * Thirty days has September,
--      April, June and November.
--      All the rest have thirty-one,
--      Saving February alone,
--      Which has twenty-eight, rain or shine.
--      And on leap years, twenty-nine.
--    * A leap year occurs on any year evenly divisible by 4, but not on a
--      century unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the twentieth century
-- (1 Jan 1901 to 31 Dec 2000)?

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

main = do putStr . show $ problem19''

-- I remember seeing this solution when working through the problems using J.
problem19 = (12 * 100) `div` 7
-- 171.

-- From greater minds than mine:
problem19' = length . filter (== sunday) . drop 12 . take 1212 $ since1900

since1900 = scanl nextMonth monday . concat $
              replicate 4 nonLeap ++ cycle (leap : replicate 3 nonLeap)

nonLeap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

leap = 31 : 29 : drop 2 nonLeap

nextMonth x y = (x+y) `mod` 7

sunday = 0
monday = 1
-- 171.

-- "Here is an alternative that is simpler, but it is cheating a bit."
problem19'' = length [ () | y <- [1901..2000],
                            m <- [1..12],
                            let (_, _, d) = toWeekDate $ fromGregorian y m 1,
                            d == 7]
-- 171.
