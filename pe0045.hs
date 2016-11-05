import Data.Bits (shiftL, shiftR) 
import Data.List
sqrtInt :: Integer -> Integer	
sqrtInt n = sqrtAlpha 1 where	
	sqrtAlpha i	
 		| i * i > n = sqrtBeta (shiftR i 1) 0	
 		| otherwise = sqrtAlpha (shiftL i 1)	
 	sqrtBeta 0 acc = acc	
 	sqrtBeta i acc = sqrtBeta (div i 2) (if (i + acc)^2 <= n then i + acc else acc)	
--isPent::Integer->Bool
--isTri::Integer->Bool
--isHex::Integer->Bool

--isPent z = let s = sqrtInt (1+24*z) in (s^2 == (1+24*z)) && (mod (1+s) 6 ==0)
--isTri z = let s = sqrtInt (2*z) in (s*(s+1)==2*z)
--isHex z = let s = sqrtInt (1+8*z) in (0==mod (1+s) 4) && ((div (1+s) 4)*(2*(div (1+s) 4) -1)==z)

--x=take 3  (filter isPent (filter isHex  [1..]))

isSqr z = let s = sqrtInt z in z==(s^2)

a=map sqrtInt (filter isSqr [12*(m^2) - 4*m +1|m<-[1..]])
n=map (\x-> div (x+1) 4) (filter (\x-> (mod x 4 )==(mod 3 4)) a)
h=map (\x->x*(2*x-1)) n
x=(take 3 h)