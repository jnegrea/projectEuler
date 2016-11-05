import Data.List
uniqueappend l1 l2
	|l1==[]=l2
	|l2==[]=l1
	|((length l1) == 1 ) && (not (l1!!0 `elem` l2)) = (l1!!0):l2
	|((length l1) == 1 ) = l2
	|otherwise = uniqueappend [(l1!!0)] (uniqueappend (drop 1 l1) l2)

collatz::Int->[(Int,Int)]->[(Int,Int)]
collatz n list
	|(elem n (fst (unzip list))) = list
	|n==1 = [(1,1)]
	|(mod n 2 ==0) && (not (null l1)) = ((fst (l1!!0))+1,n):list
	|(mod n 2 ==1) && (not (null l2)) = ((fst (l2!!0))+1,n):list
	|(mod n 2 ==0) = let ((l,m):xs)=(collatz (div n 2) list) in (l+1,n):(l,m):xs
	|(mod n 2 ==1) = let ((l,m):xs)=(collatz (3*n+1) list) in (l+1,n):(l,m):xs
	where 	
		l1=(filter (\x -> ((div n 2) == (snd x))) list)
		l2=(filter (\x -> ((3*n+1) == (snd x))) list)

order a b
	|a<b = GT
	|a>b = LT
	|a==b = EQ

bigCollatz n m
	|n==1 = m
	|True = bigCollatz (n-1) (max ((collatz n [])!!0) m)

main= putStrLn (show (bigCollatz 1000000 (1,1)))