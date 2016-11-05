import Data.Bits (shiftL, shiftR) 
import Data.List

sqrtInt n = sqrtAlpha 1 where	
	sqrtAlpha i	
 		| i * i > n = sqrtBeta (shiftR i 1) 0	
 		| otherwise = sqrtAlpha (shiftL i 1)	
 	sqrtBeta 0 acc = acc	
 	sqrtBeta i acc = sqrtBeta (div i 2) (if (i + acc)^2 <= n then i + acc else acc)	


isPent z = let s = sqrtInt (1+24*z) in (s^2 == (1+24*z)) && (mod (1+s) 6 ==0)

loPent:: [Integer]
loPent= [div (n*(3*n-1)) 2| n<-[1..]]

x=[(a,b) | a<-loPent, b<-(takeWhile (<a) loPent), (isPent (b+a)), (isPent (a-b))]

--y=[(m,n)| ]