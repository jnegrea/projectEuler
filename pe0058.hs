
import Data.List
ppp _ []=1
ppp n (x:[])
	|x*x>n = 1
	|True = if (mod n x /=0) then 1 else 0
ppp n (x:xs)
	|x*x>n = 1
	|(mod n x /=0) = ppp n xs
	|True=0
isPrime n = ppp n [2..n]

s2=[4*n^2+2*n+1| n<-[1..]]
s3=[4*n^2+0*n+1| n<-[1..]]
s4=[4*n^2-2*n+1| n<-[1..]]

p2 =map isPrime s2
p3 =map isPrime s3
p4 =map isPrime s4

x=head [n | n<-[1..], 10*((sum (take n p2))+(sum (take n p3))+(sum (take n p4)))<(1+4*n)]
y=[(((sum (take n p2))+(sum (take n p3))+(sum (take n p4))),(1+4*n)) | n<-[1..]]