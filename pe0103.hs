listSplits [] = [([],[])]
listSplits (x:xs)= [(x:l1,l2)|(l1,l2)<-ll]++[(l1,x:l2)|(l1,l2)<-ll]++[(l1,l2)|(l1,l2)<-ll]
	where ll =(listSplits xs)

noSplitEq = and . (map (\(l1,l2)-> ((sum l1) /= (sum l2))||(((sum l1)==0)&&((sum l2)==0)))) . listSplits

test = [(a1,a2,a3,a4,a5,a6,a7)|
	a1<-[1..33], 
	a2<-[(a1+1)..(div (255-a1-15) 6)],
	a3<-[(a2+1)..min (div (255-a1-a2-10) 5) (a1+a2-4)],
	a4<-[(a3+1)..min (div (255-a1-a2-a3-6) 4) (a1+a2-3)],
	noSplitEq [a1,a2,a3,a4],

	let a5' = helper5 a1 a2 a3 a4,
	a5'>a4,
	a5<-[a4+1..min (div (255-a1-a2-a3-a4-3) 3) a5'],
	noSplitEq [a1,a2,a3,a4,a5],

	let a6' = helper6 a1 a2 a3 a4 a5,
	a6'>a5,
	a6<-[a5+1..min (div (255-a1-a2-a3-a4-a5-1) 2) a6'],
	(sum [a1,a2,a3,a4,a5,a6]) >= 115,
	noSplitEq [a1,a2,a3,a4,a5,a6],


	let a7' = helper7 a1 a2 a3 a4 a5 a6,
	a7'>a6,	
	a7<-[a6+1..min a7' (255-sum [a1,a2,a3,a4,a5,a6])],
	noSplitEq [a1,a2,a3,a4,a5,a6,a7],
	True]
	where 
		helper5 b1 b2 b3 b4
			|mod s3 2 == 0 && mod s4 3 == 0 = minimum [b1+b2-2, (div s3 2) -1,(div s4 3) -1]
			|mod s3 2 /= 0 && mod s4 3 == 0 = minimum [b1+b2-2, (div s3 2),(div s4 3) -1]
			|mod s3 2 == 0 && mod s4 3 /= 0 = minimum [b1+b2-2, (div s3 2) -1,(div s4 3)]
			|otherwise = minimum [b1+b2-2, (div s3 2),(div s4 3)]
			where
				s3=(b1+b2+b3 - 3)
				s4=(b1+b2+b3+b4 - 3)
		helper6 b1 b2 b3 b4 b5
			|mod s3 2 == 0 && mod s4 2 == 0 = minimum [b1+b2-1, (div s3 2) -1,(div s4 2) -1]
			|mod s3 2 /= 0 && mod s4 2 == 0 = minimum [b1+b2-1, (div s3 2),(div s4 2) -1]
			|mod s3 2 == 0 && mod s4 2 /= 0 = minimum [b1+b2-1, (div s3 2) -1,(div s4 2)]
			|otherwise = minimum [b1+b2-1, (div s3 2),(div s4 2)]
			where
				s3=(b1+b2+b3 - 1)
				s4=(b1+b2+b3+b4-b5)
		helper7 b1 b2 b3 b4 b5 b6
			|mod s3 2 == 0 && mod s4 3 == 0 = minimum [b1+b2, s3,s4]
			|mod s3 2 /= 0 && mod s4 3 == 0 = minimum [b1+b2, s3,s4]
			|mod s3 2 == 0 && mod s4 3 /= 0 = minimum [b1+b2, s3,s4]
			|otherwise = minimum [b1+b2, s3,s4]
			where
				s3=(b1+b2+b3 -b6)
				s4=(b1+b2+b3+b4 -b5-b6)

helper5 b1 b2 b3 b4
	|mod s3 2 == 0 && mod s4 3 == 0 = minimum [b1+b2-2, (div s3 2) -1,(div s4 3) -1]
	|mod s3 2 /= 0 && mod s4 3 == 0 = minimum [b1+b2-2, (div s3 2),(div s4 3) -1]
	|mod s3 2 == 0 && mod s4 3 /= 0 = minimum [b1+b2-2, (div s3 2) -1,(div s4 3)]
	|otherwise = minimum [b1+b2-2, (div s3 2),(div s4 3)]
	where
		s3=(b1+b2+b3 - 3)
		s4=(b1+b2+b3+b4 - 3)
helper6 b1 b2 b3 b4 b5
	|mod s3 2 == 0 && mod s4 2 == 0 = minimum [b1+b2-1, (div s3 2) -1,(div s4 2) -1]
	|mod s3 2 /= 0 && mod s4 2 == 0 = minimum [b1+b2-1, (div s3 2),(div s4 2) -1]
	|mod s3 2 == 0 && mod s4 2 /= 0 = minimum [b1+b2-1, (div s3 2) -1,(div s4 2)]
	|otherwise = minimum [b1+b2-1, (div s3 2),(div s4 2)]
	where
		s3=(b1+b2+b3 - 1)
		s4=(b1+b2+b3+b4-b5)
helper7 b1 b2 b3 b4 b5 b6
	|mod s3 2 == 0 && mod s4 3 == 0 = minimum [b1+b2, s3,s4]
	|mod s3 2 /= 0 && mod s4 3 == 0 = minimum [b1+b2, s3,s4]
	|mod s3 2 == 0 && mod s4 3 /= 0 = minimum [b1+b2, s3,s4]
	|otherwise = minimum [b1+b2, s3,s4]
	where
		s3=(b1+b2+b3 -b6)
		s4=(b1+b2+b3+b4 -b5-b6)


main=do 
	print $take 10 test
	--print $helper7 20 31 38 39 40 42
	--print $(255-(sum [20,31,38,39,40])-1)/2
	--print $noSplitEq [20,31,38,39,40,42,45]
	--print $ sum [20,31,38,39,40,42,45]
	--print $ [20+31>45, 20+31+38>42+45,20+31+38+39>40+42+45]
	--print $ div (sum [20,31,38,39] +3) 3
	--print $ div (sum [20,31,38] +1) 2 -1