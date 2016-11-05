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

myPrimes =filter (>=100000) $ primes 999999

isPrime n loPrimes= n `elem` loPrimes

myBool p
	| p =1
	| otherwise =0

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



myConditionHelper p a r= ((sum [(myBool (millerRabinPrimality (p+k*a) 48561))| k<-[0..9-r]])>=8)


dig:: Int->Int->Int
dig p 0 = mod p 10
dig p n = dig (quot p 10) (n-1)

digN012list:: Int ->[Int] ->Bool
digN012list p lon
		|((and (map (==0) (map (dig p) lon)))) = True
		|((and (map (==1) (map (dig p) lon)))) = True
		|((and (map (==2) (map (dig p) lon)))) =True
		|otherwise=False

myCondition p
	|((digN012list p [1]) && (myConditionHelper p 10 (dig p 1)))= 1
	|((digN012list p [2]) && (myConditionHelper p 100 (dig p 2)))=2
	|((digN012list p [3]) && (myConditionHelper p 1000 (dig p 3)))=3
	|((digN012list p [4]) && (myConditionHelper p 10000 (dig p 4)))=4
	|((digN012list p [1,2]) && (myConditionHelper p 110 (dig p 1)))=5
	|((digN012list p [1,3]) && (myConditionHelper p 1010 (dig p 1)))=6
	|((digN012list p [2,3]) && (myConditionHelper p 1100 (dig p 2)))=7
	|((digN012list p [1,4]) && (myConditionHelper p 10010 (dig p 1)))=8
	|((digN012list p [2,4]) && (myConditionHelper p 10100 (dig p 2)))=9
	|((digN012list p [3,4]) && (myConditionHelper p 11000 (dig p 3)))=10
	|((digN012list p [1,2,3]) && (myConditionHelper p 1110 (dig p 1)))=11
	|((digN012list p [1,2,4]) && (myConditionHelper p 10110 (dig p 1)))=12
	|((digN012list p [1,3,4]) && (myConditionHelper p 11010 (dig p 1)))=13
	|((digN012list p [2,3,4]) && (myConditionHelper p 11100 (dig p 2)))=14
	|((digN012list p [1,2,3,4]) && (myConditionHelper p 11110 (dig p 1)))=15
	|((digN012list p [1,2,3,5]) && (myConditionHelper p 101110 (dig p 1)))=16
	|((digN012list p [1,3,4,5]) && (myConditionHelper p 111010 (dig p 1)))=17
	|((digN012list p [2,3,4,5]) && (myConditionHelper p 111100 (dig p 2)))=18
	|((digN012list p [5]) && (myConditionHelper p 100000 (dig p 5)))=19
	|((digN012list p [1,5]) && (myConditionHelper p 100010 (dig p 1)))=20
	|((digN012list p [2,5]) && (myConditionHelper p 100100 (dig p 2)))=21
	|((digN012list p [3,5]) && (myConditionHelper p 101000 (dig p 3)))=22
	|((digN012list p [4,5]) && (myConditionHelper p 110000 (dig p 4)))=23
	|((digN012list p [1,2,5]) && (myConditionHelper p 100110 (dig p 1)))=24
	|((digN012list p [1,3,5]) && (myConditionHelper p 101010 (dig p 1)))=25
	|((digN012list p [2,3,5]) && (myConditionHelper p 101100 (dig p 2)))=26
	|((digN012list p [1,4,5]) && (myConditionHelper p 110010 (dig p 1)))=27
	|((digN012list p [2,4,5]) && (myConditionHelper p 110100 (dig p 2)))=28
	|((digN012list p [3,4,5]) && (myConditionHelper p 111000 (dig p 3)))=29
	|otherwise = 0

myCondition2 p = (myCondition p ) >0

myCondition3 p = ((myCondition p ) >0, (myCondition p ),p)

myFilterCond (x,y,z) = x

x=head $ filter myFilterCond $ map myCondition3 myPrimes
p=((digN012list 100297 [1,2,5]))
q = (map (dig 100297) [1,2,5])
r=dig 100297 2
--x=myPrimes
--y=digN012list 13 [1] 
--z= myConditionHelper 13 10 (dig 13 1)
--w=[(myBool (isPrime (13+k*10) myPrimes))| k<-[1..9-1]]


testFun pl p = isPrime p pl
--p=map (testFun myPrimes) [10133, 11133, 12133,13133,14133,15133,16133,17133,18133,19133]
main = do
	print x
	--print y
	--print z
	--print w
	--print r
