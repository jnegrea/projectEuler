addPoly p1 p2 = if (length p1 >= length p2)
                then zipWith (+) p1 (p2 ++ repeat 0)
                else addPoly p2 p1
multiplyBy a p1 = map (a*) p1
multiplyByX p = 0:p
multPoly [] p2 = []
multPoly (p:p1) p2 = let pTimesP2 = multiplyBy p p2
                         xTimesP1Timesp2 = multiplyByX $ multPoly p1 p2
                     in addPoly pTimesP2 xTimesP1Timesp2   

carryH1 lon = map (\x->div x 10) lon
carryH2 lon = map (\x->mod x 10) lon
carry lon
	|maximum a == 0 = b
	|True = carry (zipWith (+) (0:a) b)
	where 
		a=carryH1 lon
		b=carryH2 lon