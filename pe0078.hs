
partitions':: Int->Int->[[(Int,Int)]]
partitions' 0 maxpart=[[]]
partitions' n maxpart= [(k,m):lokm|
		m<-[1..min n maxpart],
		let lowerK=if m==1 then n else 1,
		let upperK=if m==1 then n else (div n m),
		k<-[lowerK..upperK],
		lokm<-(if m==1 then [[]] else (partitions''!!(n-k*m)!!(m-1)))
		]

partitions''::[[[[(Int,Int)]]]]
partitions''=[[partitions' x y|y<-[0..]]|x<-[0..]]

partitions:: Int->(Int,Int)
partitions n = (length (partitions''!!n!!n),n)

--ans = head (filter (\(x,y)-> (x `mod` 1000000) ==0) (map (\x->(length$partitions x, x)) [1..]))

ans =(length $ partitions''!!100)-1
main =do
	print $ ans

