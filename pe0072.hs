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

primesHelper:: [Int]-> Int-> Int ->[Int]
primesHelper [] m sqrtM =[]
primesHelper [a] m sqrtM =[a]
primesHelper (a:xs) m sqrtM
    |a>(sqrtM+1) =a:xs
    |otherwise =a:(primesHelper (filter (\x-> mod x a /=0) xs) m sqrtM)

primes::Int -> [Int]
primes m = (primesHelper (2:[3,5..m]) m (squareRoot m))

unfactorized:: [(Int,Int)]->Int
unfactorized lopk = product [p^k|(p,k)<-lopk]

integerLog :: Int->Int->Int
integerLog n p = head [k | k<-[0..], p^k > n]

loIntsHelper::[Int]->[(Int,(Int,[(Int,Int)]))]->Int->[(Int,(Int,[(Int,Int)]))]
loIntsHelper [] ilist max = ilist
loIntsHelper (p:plist) ilist mx = 
  (filter (\x-> ((div mx (fst x)) < p)) ilist)++(loIntsHelper plist 
    ([(u, (v,mynum))
    |i<-(filter (\x-> ((div mx (fst x)) >= p)) ilist),
    k<-[0..((integerLog (div mx (fst i)) p) -1)],
    let mynum= if k==0 then (snd$snd i) else filter (/=(1,1)) $ sort ((p,k):(snd$snd i)),
    let u = if k==0 then (fst i) else (fst i)*p^k,
    let v = if k==0 then (fst$snd i) else (fst$snd i)*p^(k-1)*(p-1)
    ]) mx)

loInts::Int->[(Int,(Int,[(Int,Int)]))]
loInts n = filter (> (1,(1,[(1,1)]))) $loIntsHelper (primes n) [(1,(1,[(1,1)]))] n
loTots n = map (\(a,(b,c))->b) $loInts n


main = do
  print $sum$loTots 1000000