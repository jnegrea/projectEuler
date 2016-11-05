
import Data.List
oneDigPrimes=[3,5,7]
ppp _ []=True
ppp n (x:[])
		|x*x>n = True
		|True = (mod n x /=0)
ppp n (x:xs)
		|x*x>n = True
		|(mod n x /=0) = ppp n xs
		|True=False
isPrime n = ppp n [2..n]


leftTrunkPrimes 2 = [10*x+p|x<-[1,3..9],p<-oneDigPrimes, isPrime (10*x+p)]
leftTrunkPrimes n = [(10^(n-1))*x+p|x<-[1,3..9],p<-(leftTrunkPrimes (n-1)), isPrime ((10^(n-1))*x+p)]

rightTrunkPrimes 2 = [10*p+x|x<-[1,3..9],p<-oneDigPrimes, isPrime (10*p+x)]
rightTrunkPrimes n = [10*p+x|x<-[1,3..9],p<-(rightTrunkPrimes (n-1)), isPrime (10*p+x)]

trunkPrimes n = (intersect (leftTrunkPrimes n) (rightTrunkPrimes n))

f m lol
	|m==0 = []
	|True = (head lol)++(f (m-(length (head lol))) (tail lol))

x=sum (23:(f 10 [trunkPrimes n | n<-[2..]]))