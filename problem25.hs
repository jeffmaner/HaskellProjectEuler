-- The Fibonacci sequence is defined by the recurrence relation:
-- 
--     F(n) = F(n-1) + F(n-2), where F(1) = 1 and F(2) = 1.
--
--Hence the first 12 terms will be:
--
--     F(1)  =   1
--     F(2)  =   1
--     F(3)  =   2
--     F(4)  =   3
--     F(5)  =   5
--     F(6)  =   8
--     F(7)  =  13
--     F(8)  =  21
--     F(9)  =  34
--     F(10) =  55
--     F(11) =  89
--     F(12) = 144
-- 
-- The 12th term, F(12), is the first term to contain three digits.
-- 
-- What is the first term in the Fibonacci sequence to contain 1000 digits?

main = do print $ problem25''

problem25 = head $ dropWhile tooShort $ map fib [1..]
  where
    tooShort n = (length $ show n) < 1000
    fib 1 = 1
    fib 2 = 1
    fib n = fib (n-1) + fib (n-2)
-- LOL, will it ever finish?

-- From greater minds than mine:
fibs = 0:1:(zipWith (+) fibs (tail fibs)) -- This blows my mind.

problem25' = last $ takeWhile (<10^1000) fibs
-- 7334343300431526745413249208471480941295886039383885689591799012660621316779395823705306935954440374490983316440510938218661503030861630551548176293222468430475826259959800604212901428982055681829150950655199287783857015061224226795482474623367665222282256870316117093830732812895097667473105049114270817452107418279883991688450006170770502566063032564075310632474734425490689190635259549767940099905087501320929296630972279603414930997444279094039374065272740853520039785786698227588316593293922583225068083713333591230083257890903615803904675715141475535706043142298866792045298371160569065663936383352746627653451155462689725430610815806257367133656040401322717863284076339742756245203855814663113490689862210696900867531084265725317796456979714878328669946807443728887797839435463413795912940497316758611168447311213734925988635812151655063724137755355835867847379508102339675238232695173896823234061425238260852508144856714319522307480482321929193614716737343149471726333522305539123082795222023.
-- LOL, this is vastly different from the answer F# gave!
-- Besides that, I think the answer is 12 rather than 144 for length 3--4782 for length 1000, not the actual value.

-- From one of theburningmonk's F# solutions:
problem25'' = f 2 1 1
  where
    f i x y
      | y > 10^999 = (i,y)
      | otherwise  = f (i+1) y (x+y)
-- (4782,1070066266382758936764980584457396885083683896632151665013235203375314520604694040621889147582489792657804694888177591957484336466672569959512996030461262748092482186144069433051234774442750273781753087579391666192149259186759553966422837148943113074699503439547001985432609723067290192870526447243726117715821825548491120525013201478612965931381792235559657452039506137551467837543229119602129934048260706175397706847068202895486902666185435124521900369480641357447470911707619766945691070098024393439617474103736912503231365532164773697023167755051595173518460579954919410967778373229665796581646513903488154256310184224190259846088000110186255550245493937113651657039447629584714548523425950428582425306083544435428212611008992863795048006894330309773217834864543113205765659868456288616808718693835297350643986297640660000723562917905207051164077614812491885830945940566688339109350944456576357666151619317753792891661581327159616877487983821820492520348473874384736771934512787029218636250627816)
