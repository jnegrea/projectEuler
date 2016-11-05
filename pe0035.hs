import Data.List
ppp _ []=True
ppp n (x:[])
		|x*x>n = True
		|True = (mod n x /=0)
ppp n (x:xs)
		|x*x>n = True
		|(mod n x /=0) = ppp n xs
		|True=False
rotate n 0 = n
rotate n 1 = 10*(mod n (10^((ndigits n)-1)))+(div n (10^((ndigits n)-1)))
rotate n m = rotate (rotate n (m-1)) 1
intToList 0 = [];
intToList n = (mod n 10):(intToList (div (n-(mod n 10)) 10))

ndigits n=length $ intToList n
isPrime n = ppp n [2..n]
circPrime n 
	|foldl (\x y->x && (isPrime y)) True p=p
	|True=[]
	where p=[rotate n m|m<-[0..((ndigits n)-1)]]

x6=nub [p |g<-[1,3..9],f<-[1,3..g],e<-[1,3..g],d<-[1,3..g],c<-[1,3..g],b<-[1,3..g],p<-(circPrime ((10^5)*b+(10^4)*c+(10^3)*d+(10^2)*e+(10^1)*f+g))]::[Int]
x5=nub [p |g<-[1,3..9],f<-[1,3..g],e<-[1,3..g],d<-[1,3..g],c<-[1,3..g],p<-(circPrime ((10^4)*c+(10^3)*d+(10^2)*e+(10^1)*f+g))]::[Int]
x4=nub [p |g<-[1,3..9],f<-[1,3..g],e<-[1,3..g],d<-[1,3..g],p<-(circPrime ((10^3)*d+(10^2)*e+(10^1)*f+g))]::[Int]
x3=nub [p |g<-[1,3..9],f<-[1,3..g],e<-[1,3..g],p<-(circPrime ((10^2)*e+(10^1)*f+g))]::[Int]
x2=nub [p |g<-[1,3..9],f<-[1,3..g],p<-(circPrime ((10^1)*f+g))]::[Int]
x1=nub [p |p<-[2..9],isPrime p]::[Int]

z=(length x1)+(length x2)+(length x3)+(length x4)+(length x5)+(length x6)