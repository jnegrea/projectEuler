import Data.List

squareRoot :: Int -> Int
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^2 <= n && n < (r+1)^2
  in  head $ dropWhile (not . isRoot) iters

primesHelper [] m sqrtM =[]
primesHelper [a] m sqrtM =[a]
primesHelper (a:xs) m sqrtM
	|a>(sqrtM+1) =a:xs
	|otherwise =a:(primesHelper (filter (\x-> mod x a /=0) xs) m sqrtM)

primes m = (primesHelper (2:[3,5..m]) m (squareRoot m))

myPrimes = primes 9999

find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
    where 
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2        
 
-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Int -> Int -> Bool
millerRabinPrimality n a
    | a <= 1 || a >= n-1 = 
        error $ "millerRabinPrimality: a out of range (" 
              ++ show a ++ " for "++ show n ++ ")" 
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs
 
-- (eq. to) pow' (*) (^2) n k = n^k
pow' :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
    where 
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x
 
mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a
 
-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)

nDigs n 
	|n < 10 = 1
	|otherwise = 1+(nDigs (quot n 10))

mysum (p1,p2,p3,p4,p5) =p1+p2+p3+p4+p5


cond p q = (millerRabinPrimality (10^(nDigs p)*q+p) q)
cond2 p q = (cond p q)&&(cond q p)

prop1 p1 p2 = (673 <= p2) && (p2 < p1) && (cond2 p1 p2)
prop2 p1 p2 p3 = (p3 < p2) && (cond2 p1 p3) && (cond2 p2 p3)
prop3 p1 p2 p3 p4 = (p4 < p3) && (cond2 p1 p4) && (cond2 p2 p4) && (cond2 p3 p4)
prop4 p1 p2 p3 p4 p5 = (p5 >= 792-p2-p3-p4) && (p5 < p4) && (cond2 p1 p5) && (cond2 p2 p5) && (cond2 p3 p5) && (cond2 p4 p5)

mytest = [(p1,p2,p3,p4,p5)| 
	p1<-(filter (>673) myPrimes), 
	--p1<-(filter (>1) myPrimes), 	
	p2<-(filter (prop1 p1) myPrimes), 
	p3<-(filter (prop2 p1 p2) myPrimes), 
	p4<-(filter (prop3 p1 p2 p3) myPrimes), 
	p5<-(filter (prop4 p1 p2 p3 p4) myPrimes)
	--p5<-[0]
	]

x= head mytest
y = mysum x
--x=nDigs 31
main = do
	print x
	print y
	--print z
	--print w
	--print r
