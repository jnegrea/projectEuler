monlen year mo
	|mo `elem` [1,3,5,7,8,10,12] = 31
	|mo /= 2 = 30
	|(mod year 4)==0 && (not (elem (mod year 400) [100, 200,300])) = 29
	|True=28

x=[(day,(month, year)) | year<-[1900..2000],month<-[1..12],day<-[1..(monlen year month)]]
lenx=length x
x2=(zip [1..lenx] x)
y=[z | z<-x2,((mod (fst z) 7)) ==0, (fst (snd z))==1,(snd (snd (snd z)))>=1901]
leny=length y