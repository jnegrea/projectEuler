myFun:: Integer-> Integer
myFun n = (ceiling ((fromIntegral 10::Double)**((fromIntegral (n-1)::Double)/(fromIntegral n::Double))))

test =length [(x^n,x,n)|
	n<-[1..21], 
	x<-[(myFun n)..9]
	]

main = do
	print test