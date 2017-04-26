-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for
-- which,
-- a^2 + b^2 = c^2
-- 
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
-- 
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

main = do
    (putStr . show) problem9
    --(putStr . unlines) $ map show $ triplets 1000

--problem9 = head [a*b*c|a<-[1..1000], b<-[1..1000], c<-[1..1000], a<b, b<c, a^2+b^2==c^2, a+b+c==1000]
-- Very slow. Way too slow. Killing it.
-- Improving upon mine based on some facts from below.
problem9 = head [a*b*c|a<-[1..1000],
                       b<-[1..(a-1)],
                       c<-[1..(b-1)],
                       a<b, b<c,
                       a^2+b^2==c^2,
                       a+b+c==1000]
-- Still dreadfully slow. Obviously, the naive method is inadequate.
-- Prelude.head: empty list. Hmm.

-- From greater minds than mine:
triplets x = [[a,b,c] | m <- [2..limit],
                        n <- [1..(m-1)],
                        let a = m^2 - n^2,
                        let b = 2*m*n,
                        let c = m^2 + n^2,
                        a+b+c==x]
  where limit = floor . sqrt . fromIntegral $ x

problem9' = product . head . triplets $ 1000
-- 31875000 == product [375,200,425].
