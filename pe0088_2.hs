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
loIntsHelper (p:plist) ilist max = 
  (loIntsHelper plist 
    ([(w,(u, mynum))
    |i<-(filter (\x-> ((div max (fst$snd x)) >= p)) ilist),
    k<-[1..((integerLog (div max (unfactorized$snd$snd i)) p) -1)],
    let mynum= filter (/=(1,1)) $ sort ((p,k):(snd$snd i)),
    let u = unfactorized mynum,
    let w=k+ (fst i)
    ] ++(filter (\x-> ((div max (fst$snd x)) >= p)) ilist)) max)++(filter (\x-> ((div max (fst$snd x)) < p)) ilist)

loInts::Int->[(Int,(Int,[(Int,Int)]))]
loInts n = loIntsHelper (primes n) [(0,(1,[(1,1)]))] n 



addXtofirstYSinYSS:: Int-> [Int]-> [[Int]]-> [[Int]]
addXtofirstYSinYSS x ys [] = []
addXtofirstYSinYSS x ys (ys':yss)
		|ys == ys' = (x:ys):yss
		|otherwise = ys':(addXtofirstYSinYSS x ys yss)

partitions :: [Int] -> [[[Int]]]
partitions [] = [[[]]]
partitions [x] = [[[x]]]
partitions (x:xs) =  ([[x]:l:p | l:p <- partitions xs, [x]<=l] )++
                    nub ([addXtofirstYSinYSS x (y:ys) yss | yss <- (partitions xs), (y:ys)<-(nub yss), x<=y])

factors::[Int] ->[[Int]]
factors l =  (map (\x-> map product x) $ partitions l)


productSumScore::(Int,(Int,[Int]))->(Int,Int)
productSumScore (w,(u,l)) = (w+(u- sum(l)),u)

productSumMap::[(Int,(Int,[Int]))] ->[(Int,Int)]
productSumMap loi = map productSumScore loi

unfoldFactors::(Int,(Int,[(Int,Int)]))->[(Int,(Int,[Int]))]
unfoldFactors (w,(u,l)) =  [(length l,(u,l))|l<- (factors (foldr (++) [] (map (\(p,k)->(take k (repeat p))) l)))]

productSumWrap:: [(Int,(Int,[(Int,Int)]))] ->[(Int,Int)]
productSumWrap loi= productSumMap (concat (map unfoldFactors loi))

lEqFold:: [(Int,Int)]->(Int,Int)->[(Int,Int)]
lEqFold [] (a,b) = [(a,b)]
lEqFold ((p,q):loab) (a,b)
    |(a==p) = ((p,q):loab)
    |otherwise =(a,b):((p,q):loab)

rEqFold:: [(Int,Int)]->(Int,Int)->[(Int,Int)]
rEqFold [] (a,b) = [(a,b)]
rEqFold ((p,q):loab) (a,b)
    |(b==q) = ((p,q):loab)
    |otherwise =(a,b):((p,q):loab)

rAdd::Int->(Int,Int)->Int
rAdd n (a,b) = b+n

myWrap ::Int->Int
myWrap n = foldl rAdd 0 $sort $ map (\(a,b)->(b,a))$ foldl lEqFold [] $sort $ map (\(a,b)->(b,a)) $ reverse $foldl lEqFold [] $ sort $ (filter (\(a,b)-> 1<a && a <=n) $ productSumWrap  (loInts (div (4*n) 2 )))

myWrap2 ::Int->Int
myWrap2 n = foldl rAdd 0 $ foldl rEqFold [] $foldl lEqFold [] $ sort $ (filter (\(a,b)-> a <=n) $ productSumWrap  (loInts (div (4*n) 2 )))

--test0 =foldl lEqFold [] $sort $ map (\(a,b)->(b,a)) $ reverse $foldl lEqFold [] $sort $ (filter (\(a,b)-> a <=12) $ productSumWrap  (loInts (div (4*12) 2 )))
test0=myWrap 12
--test1 = length $ partitions [2,2,2,3,3,3,5,7,11,13,13]
--test2 = length $ factors [2,2,2,3,3,3,5,7,11,13,13]
--test3 = length $ nub $ factors [2,2,2,3,3,3,5,7,11,13,13]
test1 =factors [2,2,3,3]
test2 = partitions [2,2,3,3]
main = do 
		--print test0
		print test1
		print test2
