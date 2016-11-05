import  Data.List
--import Data.List.Split

--splitAt :: Int -> [a] -> ([a], [a])
splitOn' _ []     = ([], [])
splitOn' a (x:xs)
	|a == x = ([a],xs)
	|xs==[] = ([],[])
	|otherwise = let (l,r) = splitOn' a (xs) in (x:l,r)

diglist n
	|n<10 = [n]
    |otherwise = (mod n 10):(diglist (div n 10))

loSS = map (reverse. diglist) $ nub [319,680,180,690,129,620,762,689,762,318,368,710,720,710,629,168,160,689,716,731,736,729,316,729,729,710,769,290,719,680,318,389,162,289,162,718,729,319,790,680,890,362,319,760,316,729,380,319,728,716]

myOrd::[a]->[a]->Ordering
myOrd l1 l2
	|length l1 < length l2 = LT
	|length l1 > length l2 = GT
	|otherwise =EQ

pphl:: [Int]->[[Int]]->[[Int]]
pphl [] p=p
pphl x [] = [x]
pphl x [[]] = [x]
pphl (x:lod) [p:pph]
	|((length lod) == 2) &&(x:lod)!!2 `elem` snd (splitOn' ((x:lod)!!1) (snd$ splitOn' ((x:lod)!!0) (p:pph))) =[p:pph]
	|x == p = [x:q | q<- (pphl lod [pph])]
	|otherwise =[x:q|q<-(pphl lod [p:pph])] ++ [p:q | q<-(pphl (x:lod) [pph])]

pphl (x:lod) ((p:pph):lpph)
	|((length lod) == 2) &&(x:lod)!!2 `elem` snd (splitOn' ((x:lod)!!1) (snd$ splitOn' ((x:lod)!!0) (p:pph))) =[p:pph]++(pphl (x:lod) lpph)
	|x == p = [x:q | q<- (pphl lod [pph])]++(pphl (x:lod) lpph)
	|otherwise =[x:q|q<-(pphl lod [p:pph])] ++ [p:q | q<-(pphl (x:lod) [pph])]++(pphl (x:lod) lpph)



passphrases::[[Int]]->Int->[[Int]]
passphrases (lod:[]) maxlen= [lod]
passphrases (lod:llod) maxlen =sortBy myOrd $ filter (\x->(length x) <=maxlen) $concat [pphl lod [pph] |pph<-(passphrases llod maxlen)]


main = do
	print $ head $passphrases loSS 9
	--print loSS
	--print $ map (elem 0 .snd. splitOn' 8 . snd . splitOn' 1) $ pphl [6,8,0] [[3,1,9]]
	--print (splitOn' 3 [1,2,3,4,5,3])