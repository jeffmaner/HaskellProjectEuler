-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it
-- may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
--
-- We shall consider fractions like 30/50 = 3/5 to be trivial examples.
--
-- There are exactly four non-trivial examples of this type of fraction, less than one in value, and
-- containing two digits in the numerator and denominator.
--
-- If the product of these four fractions is given in its lowest common terms, find the value of the
-- denominator.

-- I don't like the burning monk's solution to this, but the second one on the Haskell wiki is pretty slick.

import Data.Ratio

main = do print $ problem33

problem33 = denominator $ product [ a%c | a<-[1..9], b<-[1..9], c<-[1..9],
                                          isCurious a b c, a/=b && a/=c]
  where isCurious a b c = ((10*a+b)%(10*b+c)) == (a%c)
-- 100.
