import Data.List

triangles::[Int]
triangles=[(quot (n*(n+1)) 2)| n<-[1..]]
myTrangles = (filter (>= 1000) (takeWhile (<=9999) triangles))

squares::[Int]
squares=[(n*n)| n<-[1..]]
mySquares = (filter (>= 1000) (takeWhile (<=9999) squares))

pentagons::[Int]
pentagons=[(quot (n*(3*n-1)) 2)| n<-[1..]]
myPentagons = (filter (>= 1000) (takeWhile (<=9999) pentagons))

hexagons::[Int]
hexagons=[(n*(2*n-1))| n<-[1..]]
myHexagons = (filter (>= 1000) (takeWhile (<=9999) hexagons))

heptagons::[Int]
heptagons=[(quot (n*(5*n -3)) 2)| n<-[1..]]
myHeptagons = (filter (>= 1000) (takeWhile (<=9999) heptagons))

octagons::[Int]
octagons=[(n*(3*n-2)) | n<-[1..]]
myOctagons = (filter (>= 1000) (takeWhile (<=9999) octagons))

firstCentiget n = mod n 100
secondCentiget n = mod (quot n 100) 100
firtNsecondM m n= (firstCentiget n)==(secondCentiget m)

myNgons n
	|n==3 = myTrangles
	|n==4 = mySquares
	|n==5 = myPentagons
	|n==6 = myHexagons
	|n==7 = myHeptagons
	|n==8 = myOctagons
	|otherwise = []

x=[(length (myNgons n)) | n<-[3..8]]
y = product x

z=[([n8,n7,n6,n5,n4,n3],perm)|
	perm<-(permutations [3,4,5,6,7]), 
	n8<-(myNgons 8), 
	n7<-(filter (firtNsecondM n8) (myNgons (perm!!0))),
	n6<-(filter (firtNsecondM n7) (myNgons (perm!!1))),
	n5<-(filter (firtNsecondM n6) (myNgons (perm!!2))),
	n4<-(filter (firtNsecondM n5) (myNgons (perm!!3))),
	n3<-(filter (firtNsecondM n4) (myNgons (perm!!4))),
	(firtNsecondM n3 n8)
	]

v=(sum (fst (z!!0)))
main = do
		print x
		print y
		print z
		print v
		--print p
		--print q