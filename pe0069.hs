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

primes1000= primes 1000

test = takeWhile (<=1000000) $ scanl (*) 1 primes1000

main=do 
	print test