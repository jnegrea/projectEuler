import qualified Data.Ratio as Rat
import Data.List 

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


data FracPoly a = FracPoly {coefficients :: [Frac a]} deriving (Eq)

instance (Eq a,Show a,Num a, Integral a) => Show (FracPoly a) where
	show p
		|(Undefined `elem` (coefficients p)) = "NA"
		|otherwise = "p(x) = " ++ (showFold (coefficients p) 0 True)
		where	showFold :: (Show(b),Num b, Integral b,Eq a,Show a,Num a, Integral a)=> [Frac a] -> b -> Bool -> String  	
			showFold [] _ _	=""
			showFold cps _ _
				|allZero cps = "0"
			showFold (0:cps) n b = (showFold (cps) (n+1) b)
			showFold (cp:cps) 0 _= show(cp)++(showFold (cps) 1 False)
			showFold (cp:cps) 1 True
				|cp== 1  = "x"++(showFold (cps) 2 False)
				|cp== -1 = "-x"++(showFold (cps) 2 False)
				|otherwise = show(cp)++"*x"++(showFold (cps) 2 False)
			showFold (cp:cps) 1 False
				|cp== 1  = "+x"++(showFold (cps) 2 False)
				|cp== -1 = "-x"++(showFold (cps) 2 False)
				|cp>0 = "+"++show(cp)++"*x"++(showFold (cps) 2 False)
				|cp<0 = "-"++show(-cp)++"*x"++(showFold (cps) 2 False)
			showFold (cp:cps) n True 
				|cp== 1  = "(x^"++show(n)++")"++(showFold (cps) 2 False)
				|cp== -1 = "-(x^"++show(n)++")"++(showFold (cps) 2 False)
				|otherwise = show(cp)++"*(x^"++show(n)++")"++(showFold (cps) (n+1) False) 
			showFold (cp:cps) n False
				|cp== 1  = "+(x^"++show(n)++")"++(showFold (cps) (n+1) False)
				|cp== -1 = "-(x^"++show(n)++")"++(showFold (cps) (n+1) False)			
				|cp>0 = "+"++show(cp)++"*(x^"++show(n)++")"++(showFold (cps) (n+1) False) 
				|cp<0 = "-"++show(-cp)++"*(x^"++show(n)++")"++(showFold (cps) (n+1) False) 
			allZero l = and (map (==0) l)

instance (Eq a,Show a,Num a, Integral a) => Num (FracPoly a) where
	(+)		FracPoly {coefficients = ps1}
			FracPoly {coefficients = ps2} 
		=	if (length ps1 >= length ps2)
                then FracPoly {coefficients = foldr (\x n -> if ((n==[])&&(x==0)) then [] else x:n) [] $ zipWith (+) ps1 (ps2 ++ repeat 0)}
                --then FracPoly {coefficients = reverse $ dropWhile (==0)$ reverse $ zipWith (+) ps1 (ps2 ++ repeat 0)}
                else (+)	FracPoly {coefficients = ps2}
							FracPoly {coefficients = ps1}
 
	(*) 	FracPoly {coefficients = []}
			FracPoly {coefficients = ps2} 
		=	(FracPoly {coefficients =[]})
	(*) 	FracPoly {coefficients = ps1}
			FracPoly {coefficients = []} 
		=	(FracPoly {coefficients =[]})		
	(*) 	FracPoly {coefficients = (p1:ps1)}
			FracPoly {coefficients = ps2} 
		=	(+) (FracPoly {coefficients =map (*p1) ps2}) (FracPoly {coefficients =0:(coefficients ((FracPoly {coefficients = ps1})*(FracPoly {coefficients = ps2})))})
			

	abs		FracPoly {coefficients = ps1} = FracPoly {coefficients = map abs ps1}

	negate	FracPoly {coefficients = ps1} = FracPoly {coefficients = map negate ps1}

	signum 	FracPoly {coefficients = []} = FracPoly {coefficients = [0]}
	signum 	FracPoly {coefficients = (p1:ps1)} = FracPoly {coefficients = [signum p1]}

	fromInteger	n =	FracPoly {coefficients = [fromIntegral n]}	


scalarMult :: (Eq a,Show a,Num a, Integral a) => (Frac a) -> (FracPoly a) -> (FracPoly a) 
scalarMult s p = FracPoly {coefficients = map (*s) (coefficients p)}

onePoly :: (Eq a,Show a,Num a, Integral a)=> FracPoly a
onePoly = FracPoly {coefficients = [1]}

idPoly :: (Eq a,Show a,Num a, Integral a)=> FracPoly a
idPoly = FracPoly {coefficients = [0,1]}

evalPoly ::(Eq a,Show a,Num a, Integral a)=> FracPoly a -> Frac a -> Frac a
evalPoly (FracPoly {coefficients = []}) f = 0
evalPoly (FracPoly {coefficients = p:ps}) f = p + f*(evalPoly (FracPoly {coefficients = ps}) f) 

op ::(Eq a,Show a,Num a, Integral a)=> FracPoly a -> a -> FracPoly a
op p 1 = scalarMult (evalPoly p 1) onePoly
op p n = sum [scalarMult 	((recip (fromIntegral (product [(j - i)|i<-(filter (/=j) [1..n])])))*(evalPoly p (fromIntegral j)))	
							(product [(idPoly - ((fromIntegral i)*onePoly))|i<-(filter (/=j) [1..n])]) 
						| j<-[1..n]]

p_n = FracPoly {coefficients = [Frac{numerator = (-1)^n, denominator=1}|n<-[0..10]]}

op_10 = [(op p_n n) |n<-[1..10]]
main = do 
	print op_10
