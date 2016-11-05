import Data.List
import qualified Data.Map as Map

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

primesHelper:: (Integral a) => [a]-> a-> a ->[a]
primesHelper [] m sqrtM =[]
primesHelper (a:xs) m sqrtM
    |a>(sqrtM+1) =a:xs
    |otherwise =a:(primesHelper (filter (\x-> mod x a /=0) xs) m sqrtM)

primes::(Integral a) => a -> [a]
primes m = (primesHelper (2:[3,5..m]) m (squareRoot m))

myPrimes = primes 1000000


primePartitions 1 _= [[]]
primePartitions n m = [(k,p):lope|
	p<-(takeWhile (<=min m n) myPrimes),
	let minK = if p==2 then (div n 2) else 1,
	let maxK = (div n p),
	k<-[minK..maxK],
	((n-p*k)/=1)&&((p/=2)||n==p*k)&&((p/=3)||(n-p*k) `mod` 2 ==0),
	lope<- if n==p*k then [[]] else primePartitionsMap Map.! ((n-p*k),(p-1))
	--lope<- if n==p*k then [[]] else primePartitions (n-p*k) (p-1)	
	]

primePartitionsMap= Map.fromList [((a,b),primePartitions a b)|a<-[0..100],b<-[2..100]]  
primePartitionsCount= [j |j<-[2..100], (length (primePartitionsMap Map.! (j,j)))>5000 ]

main = do
	print $ head$ primePartitionsCount
