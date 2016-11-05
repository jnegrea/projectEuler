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

pythTrips maxSideLen= [(k*(m*m+n*n), max (k*(m*m-n*n)) (2*k*m*n), min (k*(m*m-n*n)) (2*k*m*n))| 
	k<-[1..maxSideLen], m<-[1..maxSideLen], n<-[1..(m-1)]	,(gcd m n) ==1	,min (k*(m*m-n*n)) (2*k*m*n)<=maxSideLen, (m-n) `mod` 2 == 1	]

--x=take 10 pythTrips

tri2pythRect (r,q,p)= [(q,p-a,a)|a<-[1..(div p 2)]]++[(p,q-a,a)|a<-[1..(div q 2)], q-a<=p]

foldFun loRect tri= (tri2pythRect tri)++loRect

pythRect maxSideLen = filter (<= (maxSideLen,maxSideLen,maxSideLen)) $ foldl foldFun [] (pythTrips maxSideLen)



tri2pythRect' (r,q,p) maxSideLen
	| q<=maxSideLen = (div p 2)+(max ((div q 2) - (q-p)+1) 0)
	| p<=maxSideLen = (max ((min maxSideLen (div q 2)) - (q-p)+1) 0)
	| otherwise = 0


pythTrips' :: Int -> [Int]
pythTrips' maxSideLen= [(tri2pythRect' (k*(m*m+n*n), max (k*(m*m-n*n)) (2*k*m*n), min (k*(m*m-n*n)) (2*k*m*n)) maxSideLen)
	|k<-[1..(div maxSideLen 2)+1], m<-[1..(div maxSideLen (2*k))+1], n<-[1..(m-1)]	,(gcd m n) ==1	,min (k*(m*m-n*n)) (2*k*m*n)<=maxSideLen, (m-n) `mod` 2 == 1	]


--foldFun' maxSideLen loRect tri = (tri2pythRect' tri maxSideLen)+loRect

--pythRect' maxSideLen = foldl (foldFun' maxSideLen) 0 (pythTrips maxSideLen)

--x=(pythRect 8)
--x=(pythTrips 100 )

--y=(pythRect 100 )
--z = length y
binIntSearchHelper1 :: (Integral a0)=>(a0->Bool)->a0
binIntSearchHelper1 proposition = head [k | k<-[0..], proposition (2^k)]

binIntSearchHelper2 :: (Integral a0)=>(a0 ->Bool)->(a0)->(a0)->(a0)
binIntSearchHelper2 proposition l u
	|l == u = l
	|l+1 == u = u
	|proposition midpt = binIntSearchHelper2 proposition l midpt
	|otherwise = binIntSearchHelper2 proposition midpt u
	where midpt = l+(div (u-l) 2)

binIntSearch :: (Integral a0)=>(a0->Bool)->a0
binIntSearch proposition = binIntSearchHelper2 proposition l (2*l)
	where l = 2^((binIntSearchHelper1 proposition)-1)


prop n= (sum $ pythTrips' n) >1000000
w=(binIntSearch prop)
--w=binIntSearch (>1000)
--x= sum w
main=do 
	--print w
	print w