import Data.List
import Math.NumberTheory.Logarithms
import Math.NumberTheory.Powers
import qualified Data.Map as Map
import Data.Ord

--import Data.Map as M
intLogBase ::Int->Int->Int 
intLogBase b n = integerLogBase (fromIntegral b) (fromIntegral n) 

data  PrimePower a= PrimePower {prime::a, power::a} deriving (Ord, Eq)
instance  (Ord a,Show a, Num a)=> Show (PrimePower a) where
	show PrimePower {prime=p, power=e} 
		|e>1 = "("++show(p)++"^"++show(e)++")"
		|otherwise = "("++show(p)++")"

type Factorization a = [PrimePower a]

data MyInt a = MyInt {value::a, factorization::(Factorization a)} deriving (Ord, Eq)
instance (Ord a,Show a, Num a)=>Show (MyInt a) where
	show MyInt {value=v,factorization=[]} = show(v)++"=(1)"++"\n"
	show MyInt {value=v,factorization=pp:pps} = show(v)++"="++show(pp) ++ (foldl (\x qq -> x++"*"++show(qq)) [] pps)++"\n"

myOne ::(Integral a, Show a, Ord a, Eq a)=> MyInt a
myOne = MyInt {value=1, factorization= ([])}

fastInt :: (Integral a, Show a, Ord a, Eq a) => (a, [(a,a)]) -> MyInt a 
fastInt (v, l) = (MyInt {value=v, factorization=([(PrimePower {prime=p1, power=e1})|(p1,e1)<-l])})

insertPP_intoF ::(Integral a, Show a, Ord a, Eq a)=> PrimePower a -> Factorization a -> Factorization a
insertPP_intoF pp [] = [pp]
insertPP_intoF (PrimePower {prime=p1, power=e1}) ((PrimePower {prime=p2, power=e2}):fs)
	|p1<p2 =  (PrimePower {prime=p1, power=e1}):(PrimePower {prime=p2, power=e2}):fs
	|p1==p2 =  (PrimePower {prime=p2, power=(e1+e2)}):fs
	|otherwise =  (PrimePower {prime=p2, power=e2}):(insertPP_intoF (PrimePower {prime=p1, power=e1}) fs)

valuePP ::(Integral a, Show a, Ord a, Eq a)=> PrimePower a -> a
valuePP (PrimePower {prime=p1, power=e1}) = p1^e1

myMult ::(Integral a, Show a, Ord a, Eq a)=>  (MyInt a) -> (MyInt a) -> (MyInt a)
myMult m n
	|m == myOne = n
	|n == myOne = m 
myMult 	(MyInt {value=v1, factorization=f1}) (MyInt {value=v2, factorization=((pp2:pps2))})
	= myMult 	(MyInt {value=(v1*mvpwr), factorization=(insertPP_intoF pp2 f1)}) 
				(MyInt {value=(div v2 mvpwr), factorization=pps2})		
	where 	mvpwr = valuePP pp2

splitFilter:: (a->Bool)->[a]->([a],[a])
splitFilter p [] = ([],[])
splitFilter p (x:xs)
		|p x = (x:xs1,xs2)
		|otherwise = (xs1,x:xs2)
		where (xs1,xs2) = splitFilter p xs

loMyIntsHelper::[Int]->[MyInt Int]->Int->[MyInt Int]
loMyIntsHelper [] ilist mx = ilist
loMyIntsHelper (p:plist) ilist mx = 
  (lhs)++
  (loMyIntsHelper plist 
    (rhs++
    	([MyInt {value=u, factorization=f}|
    			(MyInt {value=u0,factorization=f0})<-(rhs),
    			k<-[1..((intLogBase p (div mx (u0))))],
    			let f = insertPP_intoF (PrimePower {prime=p, power=k}) (f0),
    			let u = u0*p^k
    ])) mx)
  where (lhs,rhs) = splitFilter (\x-> ((div mx (value x)) < p)) ilist

loMyInts:: Int->[MyInt Int]
loMyInts n =(sort$ loMyIntsHelper (primes n) [myOne] n)

myIntMap:: Int -> (Map.Map Int (MyInt Int))
myIntMap n = Map.fromList [(v,MyInt {value=v,factorization=f})| MyInt {value=v,factorization=f}<-(loMyInts n)]

primesHelper:: (Integral a) => [a]-> a-> a ->[a]
primesHelper [] m sqrtM =[]
primesHelper (a:xs) m sqrtM
    |a>(sqrtM+1) =a:xs
    |otherwise =a:(primesHelper (filter (\x-> mod x a /=0) xs) m sqrtM)

primes::(Integral a) => a -> [a]
primes m = (primesHelper (2:[3,5..m]) m (integerSquareRoot  m))

divisorX::Int-> (MyInt Int) ->Int {-(Maybe (MyInt Int))-}
divisorX x mi = product [ (div (p^((e+1)*x) -1) (p^x-1)) | PrimePower {prime=p, power =e}<- (factorization mi)] - (value mi)

unJust::Maybe a ->a
unJust =(\(Just x)->x)

myIntMapMM :: (Map.Map Int (MyInt Int))
myIntMapMM=(myIntMap 1000000)

divisor1MM :: (Maybe (MyInt Int))->(Maybe (MyInt Int))
divisor1MM Nothing=Nothing
divisor1MM (Just mi)= Map.lookup (divisorX 1 mi) myIntMapMM

amicableChain'::(Maybe (MyInt Int))->(Maybe (MyInt Int))->[Int]->(Map.Map Int Int)->[(Int,Int)]
amicableChain' firstInt currInt listSoFar mapSoFar
	|(firstInt == Nothing)  = []
	|(currInt == Nothing) = zip listSoFar (repeat 0)
	|Map.lookup (value (unJust currInt)) mapSoFar /= Nothing = zip listSoFar (repeat 0)
	|(currInt == firstInt) = zip listSoFar (repeat (length listSoFar))
	|(value(unJust currInt)) `elem` listSoFar = zip (takeWhile (/= (value(unJust currInt))) (reverse listSoFar)) (repeat 0)
	|(value (unJust currInt)) < (value (unJust firstInt)) = zip listSoFar (repeat 0) 	
	|otherwise = amicableChain' firstInt (divisor1MM currInt) ((value(unJust currInt)):listSoFar) mapSoFar

amicableChain :: (Maybe (MyInt Int))->(Map.Map Int Int)->[(Int,Int)]
amicableChain mmi mapSoFar= amicableChain' mmi (divisor1MM mmi) [value (unJust mmi)] mapSoFar


updateMapWithList :: (Ord k)=>[(k,v)]->(Map.Map k v)->(Map.Map k v)
updateMapWithList l map0 = foldr (\(k,v) m -> Map.insert k v m) map0 l



amicableChainMap' currInt mapSoFar mx
	|currInt > mx = mapSoFar
	|lookupValue /= Nothing = amicableChainMap' (currInt+1) mapSoFar mx
	|otherwise = (amicableChainMap' (currInt+1) (updateMapWithList (amicableChain (Map.lookup currInt myIntMapMM) mapSoFar) mapSoFar) mx)
	where lookupValue = Map.lookup currInt mapSoFar

amicableChainMapMM = amicableChainMap' 2 (Map.empty) 1000000

myOrd (a2,a) (b2,b)
	|a<b = GT
	|a>b = LT 
	|a2<b2 = LT
	|b2<a2 = GT
	|otherwise = EQ

test = amicableChainMapMM

main = do
	--print $ divisor1MM (Map.lookup 1336 myIntMapMM)
	--print $  amicableChain (Map.lookup 1336 myIntMapMM) Map.empty
	--print $ updateMapWithList (amicableChain (Map.lookup 1184 myIntMapMM) (Map.empty)) Map.empty
	--print $ Map.lookup 1336 myIntMapMM
	--print $ Map.lookup (value (unJust (Map.lookup 1184 myIntMapMM))) (updateMapWithList (amicableChain (Map.lookup 1184 myIntMapMM) (Map.empty)) Map.empty) /= Nothing
	--print $ zip [(value (unJust (Map.lookup 1336 myIntMapMM)))] (repeat 0)
	--print $ divisor1MM ( (Map.lookup 1336 myIntMapMM))
	--print $ divisor1MM ( (Map.lookup 1184 myIntMapMM))
	--print $ divisor1MM ( (Map.lookup 1210 myIntMapMM))
	--print $ amicableChain (Map.lookup 12496 myIntMapMM) amicableChainMapMM
	print $ head $sortBy (myOrd) (Map.toList amicableChainMapMM)

	--print $ Map.lookup 1336 amicableChainMapMM	
	--print $ updateMapWithList (amicableChain2 (Map.lookup 1336 myIntMapMM) amicableChainMapMM) amicableChainMapMM
	--print $ (Map.lookup 1336 myIntMapMM) == Nothing
	--print $ ((divisor1MM (Map.lookup 1336 myIntMapMM)) == Nothing)
	--print $ Map.lookup (value (unJust ((divisor1MM (Map.lookup 1336 myIntMapMM))))) amicableChainMapMM /= Nothing
	--print $ zip [1336] (repeat 0)
	--print $ head $ sortBy (myOrd) (Map.toList amicableChainMapMM)