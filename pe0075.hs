import Data.List

squareRoot :: (Integral a) => a -> a
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (\x->x*x) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r*r <= n && n < (r+1)*(r+1)
  in  head $ dropWhile (not . isRoot) iters


pythTrips s = [(s')|
							m<-[2..squareRoot (div s 2)],
							m_n<-dropWhile (<= 2*m-(div s (2*m)) -1) [1,3..(m-1)],
							let n=m-m_n,
							n>0,
							(gcd m n) == 1,
							k<-[1..(div s (2*m*(m+n)))],
							let m2=m*m, 
							let n2=n*n,
							let a'=(m2-n2),
							let b'=2*m*n,
							let (a,b)=(\a1 b1 -> if a1 <=b1 then (k*a1,k*b1) else (k*b1,k*a1)) a' b',
							let c = k*(m2+n2),
							let s'=(a+b+c),
							s'<=s,
							True
							]

myfold' s_1 [] = 0
myfold' s_1 (s0:[])
	|s0==s_1 		= 0
	|otherwise 		= 1
myfold' s_1 (s0:s1:los)
	|(s0==s_1) = myfold' s0 (s1:los)
	|(s0 == s1)= myfold' s1 los
	|otherwise = 1+myfold' s0 (s1:los)

myfold= (myfold' 0)

main = do
		print$ myfold$sort $pythTrips 1500000
		--print $length$sort $pythTrips 1500000
		--print [1,3..3]					