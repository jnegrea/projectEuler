--continuedFractionSqrt n = (floor $ sqrt n) :(continuedFractionSqrtHelper n)



step (a,b,c) = (fl, (a,c*fl-b,(div (a -(c*fl-b)*(c*fl-b)) c)))
	where fl = (floor (((sqrt (fromIntegral a::Double))+(fromIntegral b::Double))/(fromIntegral c::Double))) 

stepper (a,b,c) loFABC
	|r == 0 = [(f,(p,q,r))]
	|elem (f,(p,q,r)) loFABC = loFABC 
	|otherwise = (stepper (p,q,r) ((f,(p,q,r)):loFABC))
	where (f,(p,q,r)) = (step (a,b,c))

myFilter n = (length (stepper (n,0,1) [])) `mod` 2 ==0
x = length $ filter myFilter [2..10000]


--x=myFilter 4
--x=stepper (23,0,1) []
main = do 
	print x