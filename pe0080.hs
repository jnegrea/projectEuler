import Data.List
import Data.Char

diglist :: (Integral a, Show a) => a -> [Int]
diglist n = map (\x->(read [x]::Int)) (show n)

sumNdigs n = sum . (take n) . diglist

squareRoot :: Integer -> Integer
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

isSqr k = let sqrtk = (squareRoot k) in sqrtk*sqrtk==k

sumNdigsSqrt n s= sumNdigs n (squareRoot (s*10^(2*n)))
test=sum [sumNdigsSqrt 100 k |k<-[1..100], not (isSqr k)]

main = do 
	print test
