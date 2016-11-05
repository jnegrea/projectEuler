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

ndivs n= length $ divisorMultiplicity n

fourInARow n = ((ndivs n) == 4) && ((ndivs (n+1)) == 4) && ((ndivs (n+2)) == 4) && ((ndivs (n+3)) == 4)
x=head $ filter fourInARow [646..]
