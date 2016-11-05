import Data.List
ppp _ []=True
ppp n (x:[])
		|x*x>n = True
		|True = (mod n x /=0)
ppp n (x:xs)
		|x*x>n = True
		|(mod n x /=0) = ppp n xs
		|True=False
isPrime n = ppp n [2..n]

diglist9=[9,8..1]
y9 =		[a | a1<-diglist9,
				a2<-diglist9\\[a1],
				a3<-diglist9\\[a1,a2],
				a4<-diglist9\\[a1,a2,a3],
				a5<-diglist9\\[a1,a2,a3,a4],
				a6<-diglist9\\[a1,a2,a3,a4,a5],
				a7<-diglist9\\[a1,a2,a3,a4,a5,a6],			
				a8<-diglist9\\[a1,a2,a3,a4,a5,a6,a7],
				a9<-diglist9\\[a1,a2,a3,a4,a5,a6,a7,a8,2,4,6,8],
				a<- [sum (zipWith (*) (map (10^) [8,7..0]) [a1,a2,a3,a4,a5,a6,a7,a8,a9])]]
x9=take 1 [a | 	a1<-diglist9,
				a2<-diglist9\\[a1],
				a3<-diglist9\\[a1,a2],
				a4<-diglist9\\[a1,a2,a3],
				a5<-diglist9\\[a1,a2,a3,a4],
				a6<-diglist9\\[a1,a2,a3,a4,a5],
				a7<-diglist9\\[a1,a2,a3,a4,a5,a6],			
				a8<-diglist9\\[a1,a2,a3,a4,a5,a6,a7],
				a9<-diglist9\\[a1,a2,a3,a4,a5,a6,a7,a8,2,4,6,8],
				a<- [sum (zipWith (*) (map (10^) [8,7..0]) [a1,a2,a3,a4,a5,a6,a7,a8,a9])],
				isPrime a]

diglist8=[8,7..1]
x8=take 1 [a | 	a1<-diglist8,
				a2<-diglist8\\[a1],
				a3<-diglist8\\[a1,a2],
				a4<-diglist8\\[a1,a2,a3],
				a5<-diglist8\\[a1,a2,a3,a4],
				a6<-diglist8\\[a1,a2,a3,a4,a5],
				a7<-diglist8\\[a1,a2,a3,a4,a5,a6],			
				a8<-diglist8\\[a1,a2,a3,a4,a5,a6,a7,2,4,6,8],
				a<- [sum (zipWith (*) (map (10^) [7,6..0]) [a1,a2,a3,a4,a5,a6,a7,a8])],
				isPrime a]

diglist7=[7,6..1]
x7=take 1 [a | 	a1<-diglist7,
				a2<-diglist7\\[a1],
				a3<-diglist7\\[a1,a2],
				a4<-diglist7\\[a1,a2,a3],
				a5<-diglist7\\[a1,a2,a3,a4],
				a6<-diglist7\\[a1,a2,a3,a4,a5],
				a7<-diglist7\\[a1,a2,a3,a4,a5,a6,2,4,6],			
				a<- [sum (zipWith (*) (map (10^) [6,5..0]) [a1,a2,a3,a4,a5,a6,a7])],
				isPrime a]

diglist6=[6,5..1]
x6=take 1 [a | 	a1<-diglist6,
				a2<-diglist6\\[a1],
				a3<-diglist6\\[a1,a2],
				a4<-diglist6\\[a1,a2,a3],
				a5<-diglist6\\[a1,a2,a3,a4],
				a6<-diglist6\\[a1,a2,a3,a4,a5,2,4,6],
				a<- [sum (zipWith (*) (map (10^) [5,4..0]) [a1,a2,a3,a4,a5,a6])],
				isPrime a]

diglist5=[5,4..1]
x5=take 1 [a | 	a1<-diglist5,
				a2<-diglist5\\[a1],
				a3<-diglist5\\[a1,a2],
				a4<-diglist5\\[a1,a2,a3],
				a5<-diglist5\\[a1,a2,a3,a4,2,4],
				a<- [sum (zipWith (*) (map (10^) [4,3..0]) [a1,a2,a3,a4,a5])],
				isPrime a]

diglist4=[4,3..1]
x4=take 1 [a | 	a1<-diglist4,
				a2<-diglist4\\[a1],
				a3<-diglist4\\[a1,a2],
				a4<-diglist4\\[a1,a2,a3,2,4],
				a<- [sum (zipWith (*) (map (10^) [3,2..0]) [a1,a2,a3,a4])],
				isPrime a]
y4= [(a1,a2,a3) | 	a1<-diglist4,
				a2<-diglist4\\[a1],
				a3<-diglist4\\[a1,a2]
				--a4<-diglist4\\[a1,a2,a3,2,4],
				--a<- [sum (zipWith (*) (map (10^) [3,2..0]) [a1,a2,a3,a4])]
				]
x=take 1 (concat ([x9,x8,x7,x6,x5,x4]))