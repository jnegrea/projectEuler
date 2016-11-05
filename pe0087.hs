import Data.List  
primes:: [Int]->[Int]
primes []=[]
primes [a]=[a]
primes (a:xs)=a:(primes (filter (\x-> mod x a /=0) xs))
primesList=reverse $ primes [2..7071]
primesList2=reverse $ primes [2..84]
primesList3=reverse $ primes [2..368]

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


cubeRoot :: Int -> Int
cubeRoot 0 = 0
cubeRoot n
    | n < c || c < 0    = r-1
    | 0 < d && d < n    = r+1
    | otherwise         = r
      where
        x = fromIntegral n :: Double
        r = truncate (x ** (1/3))
        c = r*r*r
        d = c+3*r*(r+1)

myList=[a^2+b^3+c^4|c<-primesList2, b<-(filter (<=(cubeRoot (50000000-4-c^4))) primesList3), a<-(filter (<=(squareRoot (50000000-c^4-b^3))) primesList)]

myFoldFun [] m = [m]
myFoldFun (n:lon) m
	|n==m = n:lon
	|otherwise = m:n:lon
x=foldl myFoldFun [] $ sort myList
y=length x
--z=length $ nub (take 100000 x)
w=take 100 x
--z=length $ nub x
main=do 
	--print w
	--print x
	print y
	--print z