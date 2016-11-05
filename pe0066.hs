import Data.List
import Data.Bits (shiftL, shiftR) 

sqrtInt :: Integer -> Integer	
sqrtInt n = sqrtAlpha 1 where	
	sqrtAlpha i	
 		| i * i > n = sqrtBeta (shiftR i 1) 0	
 		| otherwise = sqrtAlpha (shiftL i 1)	
 	sqrtBeta 0 acc = acc	
 	sqrtBeta i acc = sqrtBeta (div i 2) (if (i + acc)^2 <= n then i + acc else acc)	


--qRs d = nub [mod (a^2) d|a<-[0..(d-1)]]
--f d = head [a*d+b | a<-[1..],b<-[-1,1], (sqrtInt (div ((a*d+b)^2-1) d))^2==(div ((a*d+b)^2-1) d)]               
--x = minimum [f d|d<-[1..7],(sqrtInt d)^2/=d]
	


fracEq (an,ad) (bn,bd) = (an*bd==ad*bn)
simplify (an,ad) = let g=gcd an ad in ((div an g),(div ad g))
fracAdd (an,ad) (bn,bd)= (an*bd+ad*bn, ad*bd)
fracMult (an,ad) (bn,bd)=(an*bn,ad*bd)
fracSub (an,ad) (bn,bd) = fracAdd (an,ad) (-bn,bd)
fracDiv (an,ad) (bn,bd)=fracMult (an,ad) (bd,bn)


cf2f:: [Integer]->(Integer,Integer)
cf2f [a] = (a ,1)
cf2f lint= simplify ((head lint),1) `fracAdd` ((1,1) `fracDiv` (cf2f (tail lint)))

f2cf (a,1) = [a] 
f2cf (a,b) = (div a b):(f2cf (simplify (b,mod a b)))

continuedFractionSqrt s = let rs=sqrtInt s in rs:(continuedFractionSqrtHelp s (rs,0,1,rs))
continuedFractionSqrtHelp s (aold,mold,dold,rs) = anew:(continuedFractionSqrtHelp s (anew,mnew,dnew,rs))
	where 
		mnew=dold*aold - mold
		dnew=div (s-mnew*mnew) dold
		anew=(div (rs+mnew) dnew)

--cfsqrt2=(continuedFractionSqrt 2) 1000
--fst' (a,_,_,_)=a
--f d = head [a |n<-[1..], (a,b) <- [(cf2f (reverse (fst' (continuedFractionSqrt d n) )))], 
--			((a^2) - d*(b^2) )==1]

g d = head [a |n<-[1..], (a,b)<-[(cf2f (take n loa))], ((a^2)-d*(b^2))==1 ]
	where loa=continuedFractionSqrt d



x = snd $ maximum [(g d,d)|d<-[1..1000],(sqrtInt d)^2/=d]

main = do print x