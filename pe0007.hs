nthPrime n biglist
	|n == 1 = biglist!!0
	|otherwise = nthPrime (n-1) [a | a<-biglist, mod a (biglist!!0) /=0]
