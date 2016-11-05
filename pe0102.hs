import qualified Data.Ratio as Rat
import Data.List 
import Data.List.Split

data Frac a = Undefined | Frac {numerator :: a, denominator :: a}

instance (Eq a,Show a,Num a, Integral a) => Show (Frac a) where
	show Undefined  = "NA"
	show Frac {numerator =n, denominator =1} = show(n)
	show Frac {numerator =n, denominator =d} = show(n)++"/"++show(d)

instance (Eq a,Show a,Num a, Integral a) => Eq (Frac a) where
	(==)	Undefined Undefined = True
	(==)	Undefined _ = False
	(==)	_ Undefined = False
	(==)	Frac {numerator =n1, denominator =d1} 
			Frac {numerator =n2, denominator =d2}
		= (==) (n1*d2) (n2*d1)

instance (Eq a,Show a,Num a, Integral a) => Ord (Frac a) where
	compare	Undefined Undefined = EQ
	compare	Undefined _ = LT
	compare	_ Undefined = GT
	compare Frac {numerator =n1, denominator =d1} 
			Frac {numerator =n2, denominator =d2}
		= compare (n1*d2) (n2*d1)

instance (Eq a,Show a,Num a, Integral a) => Num (Frac a) where
	(+)		Undefined _ = Undefined
	(+)		_ Undefined = Undefined	
	(+)		Frac {numerator =n1, denominator =d1} 
			Frac {numerator =n2, denominator =d2} 
		= 	Frac {numerator =n3, denominator =d3}  
		where 	n3' = (n1*d2+n2*d1)
			d3' = d1*d2
			g 	= gcd n3' d3'
			n3 	= div n3' g
			d3 	= div d3' g

	(*)		Undefined _ = Undefined
	(*)		_ Undefined = Undefined					
	(*)		Frac {numerator =n1, denominator =d1} 
			Frac {numerator =n2, denominator =d2}
		= 	Frac {numerator =n3, denominator =d3}  
		where 	n3' = n1*n2
			d3' = d1*d2
			g 	= gcd n3' d3'
			n3 	= div n3' g
			d3 	= div d3' g		

	abs		Undefined =	Undefined
	abs		Frac {numerator =n1, denominator =d1}
		=	Frac {numerator = abs n1, denominator = abs d1}

	negate	Undefined = Undefined
	negate	Frac {numerator =n1, denominator =d1}
		=	Frac {numerator =(negate n1), denominator =d1}

	signum 	Undefined = Frac {numerator =0, denominator =1}
	signum	Frac {numerator =n1, denominator =d1}
		=	Frac {numerator =signum(n1)*signum(d1), denominator =1}

	fromInteger	n
		=	Frac {numerator =fromInteger(n), denominator =1}	

instance (Eq a,Show a,Num a, Integral a) => Fractional (Frac a) where
	fromRational r
		=	Frac {numerator =n1, denominator =d1}
		where 	n1'	=fromIntegral (Rat.numerator r)
			d1' = fromIntegral (Rat.denominator r)
			g 	= gcd n1' d1'
			n1 	= div n1' g
			d1 	= div d1' g

	recip	Undefined = Undefined
	recip	Frac {numerator =0, denominator =d1} 
		= 	Undefined
	recip	Frac {numerator =n1, denominator =d1} 
		= 	Frac {numerator =signum(n1)*d1, denominator =signum(n1)*n1} 



slope (p1,p2) (q1,q2) = (fromIntegral (q2-p2)::(Frac Int))/(fromIntegral (q1-p1)::(Frac Int))

cos2Theta (p1,p2) (q1,q2) (r1,r2) = 
	(fromIntegral (signCos*((q1-p1)*(r1-p1)+(q2-p2)*(r2-p2))^2) ::(Frac Integer)) /(fromIntegral (((q1-p1)^2+(q2-p2)^2) * ((r1-p1)^2+(r2-p2)^2)) ::(Frac Integer))
	where signCos = (signum ((q1-p1)*(r1-p1)+(q2-p2)*(r2-p2)))
between r p q = (p<=r && r<= q)||(p>=r && r>= q)

isInInterior :: (Int,Int)->(Int,Int)->(Int,Int)->(Int,Int)->Bool
isInInterior x a b c= ((cos2Theta a b x) >= (cos2Theta a b c))&&((cos2Theta b c x) >= (cos2Theta b c a))&&((cos2Theta c a x) >= (cos2Theta c a b))

a=(-340,495)
b=(-153,-910)
c=(835,-947)

x=(-175,41)
y=(-421,-714)
z=(574,-645)

o=(0,0)

{-main = do
	print $ isInInterior (0,0) a b c
	print $ isInInterior (0,0) (-175,41) (-421,-714) (574,-645)
	print (cos2Theta a b x)
	print (cos2Theta a b c)
	print (cos2Theta b c x)
	print (cos2Theta b c a)
	print (cos2Theta c a x)
	print (cos2Theta c a b)
	print [89438625125/448467,5043325690225/5468983,-3240227929/1882398,-56046301081/2986507,6819650210916/5456233,737206388258/2218751]
-}

readMat :: String -> [[Int]]
readMat = (map (map (read :: String -> Int)) . map (splitOn "," )) . words
main :: IO()
main = do
    rawList <- readFile "p102_triangles.txt"
    let numList =sum$ map (\[a1,a2,b1,b2,c1,c2] -> if (isInInterior (0,0) (a1,a2) (b1,b2) (c1,c2)) then 1::Int else 0::Int) $ readMat rawList
    print $ numList
    print $ isInInterior o a b c
    print $ isInInterior o x y z 
    --print numList