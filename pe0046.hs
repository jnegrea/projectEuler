import Data.List  
import Data.Bits
sqrtInt:: Integer->Integer
sqrtInt n = sqrtAlpha 1 where	
	sqrtAlpha i	
 		| i * i > n = sqrtBeta (shiftR i 1) 0	
 		| otherwise = sqrtAlpha (shiftL i 1)	
 	sqrtBeta 0 acc = acc	
 	sqrtBeta i acc = sqrtBeta (div i 2) (if (i + acc)^2 <= n then i + acc else acc)	


primes:: [Integer]->[Integer]
primes []=[]
primes [a]=[a]
primes (a:xs)=a:(primes (filter (\x-> mod x a /=0) xs))

primes1=(primes (2:[3,5..]))
sps=[2*(x^2)+p | x<-[1..], p<- (primes (2:[3,5..]))]
isSqr x = let s=(sqrtInt x) in (s^2)==x


ocs=[x*y|x<-[3,5..], y<-[3,5..x]]
isSps x = (or (map (\z-> isSqr (div (x-z) 2)) (takeWhile (<x) primes1))) ==False

xs=head $ filter isSps ocs