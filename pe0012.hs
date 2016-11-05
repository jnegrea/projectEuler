ppp n []=n
ppp n (x:[])
		|x*x>n = n
		|(mod n x /=0) = n
		|True = x
ppp n (x:xs)
		|x*x>n = n
		|(mod n x /=0) = ppp n xs
		|True=x 


divisorMultiplicityH:: Int ->[(Int,Int)]->[(Int,Int)]
divisorMultiplicityH n []
		|n==1 =[]
		|True=divisorMultiplicityH (div n x) [(1,x)]
		where x= ppp n [2..n]
divisorMultiplicityH n ((y1,y2):[])
		|n==1 =((y1,y2):[])
		|(y2 == x) = divisorMultiplicityH (div n x) ((y1+1,y2):[])
		|True= divisorMultiplicityH (div n x) ((1,x):((y1,y2):[]))
		where x= ppp n [2..n]
divisorMultiplicityH n ((y1,y2):ys)
		|n==1 =((y1,y2):ys)
		|(y2 == x) = divisorMultiplicityH (div n x) ((y1+1,y2):ys)
		|True= divisorMultiplicityH (div n x) ((1,x):((y1,y2):ys))
		where x= ppp n [2..n]

divisorMultiplicity:: Int ->[(Int,Int)]
divisorMultiplicity n = divisorMultiplicityH n []

divMltPrd [] b =b
divMltPrd a [] =a
divMltPrd ((x1,x2):xs) ((y1,y2):ys)
		|x2==y2 = (x1+y1,x2):(divMltPrd xs ys)
		|x2>y2 = (x1,x2):(divMltPrd xs ((y1,y2):ys))
		|x2<y2 = (y1,y2):(divMltPrd ((x1,x2):xs) ys)

smallestEven= [(div n 2)*(n-1)| n<- [2,4..], (product (map (+1) (fst (unzip (divMltPrd (divisorMultiplicity (div n 2)) (divisorMultiplicity (n-1)))))))>500 ]!!0
smallestOdd= [(div (n-1) 2)*n| n<- [3,5..], (product (map (+1) (fst (unzip (divMltPrd (divisorMultiplicity (div (n-1) 2)) (divisorMultiplicity (n)))))))>500 ]!!0
smallest = min smallestEven smallestOdd