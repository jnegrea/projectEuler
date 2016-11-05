import Data.Char
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

diglist :: (Integral a, Show a) => a -> [Int]
diglist n = map (\x->(read [x]::Int)) (show n)


factorial :: (Integral a, Show a, Num a) => Int -> a
factorial 0 = 1
factorial n = (fromIntegral n)*(factorial (n-1))

digFacSum :: (Integral a, Show a) => a -> a
digFacSum = sum . (map factorial) . diglist

digFacSumChain' :: (Integral a, Show a) => a -> [a]
digFacSumChain' n0 = n0:[(digFacSum n1) | n1<-(digFacSumChain' n0)]


dropRepeating' :: (Set.Set Int)->[Int]->(Map.Map Int Int)->([Int],Int)
dropRepeating' sx (x:xs) map0	|v /= Nothing = (replicate ((\(Just t) ->t) v) x, 0)
								|x `Set.member` sx 	= ([],x)
								|otherwise			= ((:) x ls,rs)
								where 		(ls,rs) = (dropRepeating' (Set.insert x sx) xs map0)
										v =Map.lookup x map0

dropRepeating :: [Int]->(Map.Map Int Int)->([Int],[Int])
dropRepeating ys map0= let (xs,x) = (dropRepeating' Set.empty ys map0) in (xs,dropWhile (/=x) xs)

digFacSumChain :: Int ->(Map.Map Int Int)-> ([Int],[Int])
digFacSumChain = dropRepeating . digFacSumChain'

makeMap1 :: ([Int],[Int])->(Map.Map Int Int)
makeMap1 (xs,ys) = Map.fromList $ (zip (take (nx-ny) xs) [nx,nx-1..ny+1])++[(y,ny)|y<-ys]
	where	nx=(length xs)
		ny=(length ys)

updateMap1 :: (Map.Map Int Int)->[Int]->Int->(Map.Map Int Int)
updateMap1 map0 [] ny = map0
updateMap1 map0 (y:ys) ny = Map.insert y ny (updateMap1 map0 ys ny) 


updateMap:: (Map.Map Int Int)->([Int],[Int])->(Map.Map Int Int)
updateMap map0 (xs,ys) | map0==(Map.fromList []) = makeMap1 (xs,ys)
updateMap map0 ([],[]) = map0 
updateMap map0 ([],y:ys) 
	|y `Map.member` map0 = map0
	|otherwise = updateMap1 map0 (y:ys) (length (y:ys))
updateMap map0 (x:xs,ys) 
	|x `Map.member` map0 = map0
	|xs == ys = updateMap map0 ([],ys) 
	|otherwise = Map.insert x (length (x:xs)) (updateMap map0 (xs,ys)) 

bigMap 1 = Map.fromList [(1,1)]
bigMap m = updateMap map0 (digFacSumChain (m-1) map0)
		where map0=bigMap (m-1)
main=do 
	--print $ myMainF 60 [1..1000]
	print $ Map.foldl (\n x->if x/=60 then n else n+1) 0 (bigMap 1000000)
