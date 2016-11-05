factorial m = product [1..m]

findperm1 n m
	|m==1 =[0]
	|True=(div n mbang):(findperm1 (mod n mbang) (m-1))
	where 	mbang=factorial (m-1)

remapperm (a:loa) (lon)
	|loa==[] = lon
	|True=let n=(lon!!a) in n:(remapperm loa (filter (\x-> x/=n) lon))

main= remapperm (findperm1 (1000000-1) 10) [0..9]