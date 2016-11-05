import Data.Char

f k
	|k==0 =2
	|k==1 =1
	|k==2 =2
	|k `mod` 3 ==2 = 2*((div (k+1) 3))
	|otherwise = 1

cfE n= [(f k)|k<-[0..n]]

int2rat n = (n,1)

ratSum (a,b) (c,d) = (a*(quot d g)+c*(quot b g), (quot (b*d) g))
	where g = (gcd b d)

ratMult (a,b) (c,d) = (quot (a*c) g, quot (b*d) g)
	where g = gcd (a*c) (b*d)

ratDiv (a,b) (c,d) = ratMult (a,b) (d,c)

ratRecip (a,b)= (b,a)

convergent (a:[])=int2rat a
convergent (a1:(a2:cf))= (ratSum (int2rat a1) (ratRecip (convergent (a2:cf))))

x= convergent $ cfE 9999
--x= [cfE n|n<-[0..10]]
sumDigs = sum . map digitToInt . show

y= sumDigs (fst x)
main = do
	print x
	print y
	