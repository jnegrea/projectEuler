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
unfactorized:: [(Int,Int)]->Int
unfactorized lopk = product [p^k|(p,k)<-lopk]

integerLog :: Int->Int->Int
integerLog n p = head [k | k<-[0..], p^k > n]

totient:: [(Int,Int)]->Int
totient lopk = product [(p-1)*p^(k-1)|(p,k)<-lopk,p>1]

loInts::[Int]->[(Int,(Int,[(Int,Int)]))]->Int->[(Int,(Int,[(Int,Int)]))]
loInts [] ilist max = ilist
loInts (p:plist) ilist max = 
  (loInts plist 
    ([(w,(u, mynum))
    |i<-(filter (\x-> ((div max (fst$snd x)) >= p)) ilist),
    k<-[1..((integerLog (div max (unfactorized$snd$snd i)) p) -1)],
    let mynum= filter (/=(1,1)) $ sort ((p,k):(snd$snd i)),
    let u = unfactorized mynum,
    let w=k+ (fst i)
    ] ++(filter (\x-> ((div max (fst$snd x)) >= p)) ilist)) max)++(filter (\x-> ((div max (fst$snd x)) < p)) ilist)

intsFactored24k = loInts (primes 24000) [(0,(1,[(1,1)]))] 24000 

intsFactored n = loInts (primes (2*n)) [(0,(1,[(1,1)]))] (n )

intsFactored24 = loInts (primes 24) [(0,(1,[(1,1)]))] 24 

composite_k k loi = filter (\x ->(fst x) >= k) loi

productSumScoreHelper_k k (w,(u,(p,m):i))
    |k == 2 =p+(div u p)
    |m > 1  = p+ (productSumScoreHelper_k (k-1) (w-1,((div u p),(p,m-1):i)))
    |otherwise  = p+ (productSumScoreHelper_k (k-1) (w-1,((div u p),i)))

productSumScoreHelper2_k k (w,(u,[])) = u

productSumScoreHelper2_k k (w,(u,p:l))
    |k<=1 = u
    |k == 2 =p+(div u p)
    |otherwise  = p+ (productSumScoreHelper2_k (k-1) (w-1,((div u p),l)))

productSumScore_k k (w,(u,(p,m):i)) = k+(u- (productSumScoreHelper_k k (w,(u,(p,m):i))))

productSumScore2_k k (w,(u,l)) = k+(u- (productSumScoreHelper2_k k (w,(u,l))))

productSumScore2All_k::Int-> (Int,(Int,[Int]))->[(Int,Int)]
productSumScore2All_k k (w,(u,l)) = map (\x-> (productSumScore2_k k x, fst$snd x)) [(w,(u,(nub (take k l))))] 

--x = productSumScore_k 2 (2,(6,([(2,1),(3,1)])))
partitions :: [Int] -> [[[Int]]]
partitions [] = [[[]]]
partitions [x] = [[[x]]]
partitions (x:xs) = [[x]:p | p <- partitions xs] ++
                    [(x:ys):yss | (ys:yss) <- partitions xs]

factors::[Int] ->[[Int]]
factors l =  (map (\x-> map product x) $ nub $ partitions l)
factors2::[Int] ->[[Int]]
factors2 l =  (map (\x-> map product x) $ nub $ map (\x-> map sort x) $ partitions l)

productSumMap2_k::Int->[(Int,(Int,[Int]))] ->[(Int,Int)]
productSumMap2_k k loi
    |k==2 = (concat (map (\(w,(u,l))->(productSumScore2All_k 2 (w,(u,l)))) ((filter (\x->(fst x)==2) loi))))
    |otherwise =  (productSumMap2_k (k-1) loi)++(concat (map (\(w,(u,l))->(productSumScore2All_k k (w,(u,l)))) ((filter (\x->(fst x)==k) loi))))

unfoldFactors::(Int,(Int,[(Int,Int)]))->[(Int,(Int,[Int]))]
unfoldFactors (w,(u,l)) =  [(length l,(u,l))|l<- (factors (foldr (++) [] (map (\(p,k)->(take k (repeat p))) l)))]

productSumMap3_k:: Int->[(Int,(Int,[(Int,Int)]))] ->[(Int,Int)]
productSumMap3_k k loi= productSumMap2_k k (concat (map unfoldFactors loi))


myFoldFun [] (a,b) = [(a,b)]
myFoldFun ((p,q):loab) (a,b)
    |(a==p) = ((p,q):loab)
    |otherwise =(a,b):((p,q):loab)

myFoldFun2 [] (a,b) = [(a,b)]
myFoldFun2 ((p,q):loab) (a,b)
    |(b==q) = ((p,q):loab)
    |otherwise =(a,b):((p,q):loab)

myFoldFun3 n (a,b) = b+n

myWrap n = foldl myFoldFun3 0 $sort $ map (\(a,b)->(b,a))$ foldl myFoldFun [] $sort $ map (\(a,b)->(b,a)) $ reverse $foldl myFoldFun [] $ sort $ (filter (\(a,b)-> a <=n) $ productSumMap3_k ((integerLog n 2)*2) (intsFactored (div (4*n) 2 )))

--test = foldl myFoldFun3 0 $ foldl myFoldFun2 [] $ reverse $foldl myFoldFun [] $ sort $ (filter (\(a,b)-> a <=12000) $   productSumMap3_k 15 intsFactored24k)
--test = foldl myFoldFun3 0 $ foldl myFoldFun2 [] $ reverse $foldl myFoldFun [] $ sort $ (filter (\(a,b)-> a <=12) $  productSumMap3_k 15 intsFactored24)
--test = sort $ map (\(a,b)->(b,a))$ foldl myFoldFun [] $sort $ map (\(a,b)->(b,a)) $ reverse $foldl myFoldFun [] $ sort $ (filter (\(a,b)-> a <=12) $ productSumMap3_k (integerLog 12 2) (intsFactored (div (4*12) 2 )))

--test = sort $ map (\(a,b)->(b,a))$ foldl myFoldFun [] $sort $ map (\(a,b)->(b,a)) $reverse $foldl myFoldFun [] $ sort $ filter (\(a,b)-> a <=120) $ productSumMap3_k ((integerLog 120 2)*2) (intsFactored 240)
--test = sort $ map (\(a,b)->(b,a))$ foldl myFoldFun [] $sort $ map (\(a,b)->(b,a)) $ reverse $foldl myFoldFun [] $sort $ filter (\(a,b)-> a <=12) $ productSumMap3_k ((integerLog 12 2)) (intsFactored 24)

test = productSumScore2All_k 2 (4,(16,[2,2,2,2]))
test1 = productSumScore2All_k 3 (4,(16,[2,2,2,2]))
test2 = productSumScore2All_k 4 (4,(16,[2,2,2,2]))
test3 = productSumScore2All_k 5 (4,(16,[2,2,2,2]))
--test = factors [2,2,3,5]
--test1 =factors2 [2,2,3,5]
--test = unfoldFactors (4,(16,([(2,4)])))
--test=myWrap 12
--test = factors $snd$snd $unfoldFactors (0,(0,[(2,7),(3,1),(5,3)]))
main = do 
    print test
    print test1
    print test2
    print test3