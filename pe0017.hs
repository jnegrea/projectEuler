spellItOut1 n
	|n==0								= 0
	|n==1	||n==2	||n==6||n==10 		= 3
	|n==4	||n==5	||n==9				= 4
	|n==3	||n==7	||n==8 				= 5
	|n==11	||n==12	||n==20				= 6
	|n==15	||n==16						= 7
	|n==13	||n==14	||n==19||n==18		= 8
	|n==17								= 9

spellItOut2 n
	|n<=20								=spellItOut1 n
	|d==2||d==3||d==9||d==8				=6+(spellItOut1 m)
	|d==4||d==5||d==6					=5+(spellItOut1 m)
	|d==7								=7+(spellItOut1 m)
	where (d,m)=((div n 10),(mod n 10))

spellItOut3 n
	|n<=99  										= spellItOut2 n
	|(m/=0) &&	(d==1	||d==2	||d==6	||d==10) 	= 3+7+3+(spellItOut2 m)
	|(d==1	||d==2	||d==6	||d==10) 				= 3+7+(spellItOut2 m)
	|(m/=0) &&	(d==4	||d==5	||d==9)				= 4+7+3+(spellItOut2 m)
	|(d==4	||d==5	||d==9)							= 4+7+(spellItOut2 m)
	|(m/=0) &&	(d==3	||d==7	||d==8)				= 5+7+3+(spellItOut2 m)
	|(d==3	||d==7	||d==8) 						= 5+7+(spellItOut2 m)
	where (d,m)=((div n 100),(mod n 100))



sumspell = foldl(\x y-> x+(spellItOut3 y)) 11 [1..999]