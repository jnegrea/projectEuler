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

explode n 
  |n <10 = [n]
  |otherwise = (mod n 10):(explode (quot n 10))

isPerm m n = (sort (explode m))==(sort (explode n))

primes10000= primes 10000

unfactorized:: [(Int,Int)]->Int
unfactorized lopk = product [p^k|(p,k)<-lopk]

integerLog :: Int->Int->Int
integerLog n p = head [k | k<-[0..], p^k > n]

loInts::[Int]->[(Double,(Int,(Int,[(Int,Int)])))]->Int->[(Double,(Int,(Int,[(Int,Int)])))]
loInts [] ilist max = ilist
loInts (p:plist) ilist max = 
  loInts plist 
    ([(r,(u, (t,mynum)))
    |i<-(filter (\x->((length$snd$snd$snd x) <3)) ilist),
    k<-[1..((integerLog (div max (unfactorized$snd$snd$snd i)) p) -1)],
    let mynum=(p,k):(snd$snd$snd i),
    let u=unfactorized mynum,
    let t=totient mynum,
    let r=(fromIntegral u::Double)/(fromIntegral t::Double),
    r<=1.00135
    ] ++ilist) max

totient lopk = product [(p-1)*p^(k-1)|(p,k)<-lopk,p>1]

loInt =loInts (takeWhile (>740) (reverse primes10000)) [(1.0,(1,(1,[(1,1)])))] 10000000


loIntTot = filter (\(_,(a,(b,_)))-> isPerm a b) $ loInt

--test = map (\x->(unfactorized x,totient x)) $ loInt
test = sort loIntTot
--test = isPerm 87109 79180

main=do 
	print test
  --print (integerLog (div 10000000 2) 3)