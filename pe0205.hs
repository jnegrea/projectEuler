
addPoly p1 p2 = if (length p1 >= length p2)
                then zipWith (+) p1 (p2 ++ repeat 0)
                else addPoly p2 p1
multiplyBy a p1 = map (a*) p1
multiplyByX p = 0:p
multPoly [] p2 = []
multPoly (p:p1) p2 = let pTimesP2 = multiplyBy p p2
                         xTimesP1Timesp2 = multiplyByX $ multPoly p1 p2
                     in addPoly pTimesP2 xTimesP1Timesp2   
                      
p1=[0,1,1,1,1]
c1=[0,1,1,1,1,1,1]
p2=multPoly p1 p1
p4=multPoly p2 p2
p8=multPoly p4 p4
p9=multPoly p8 p1
c2=multPoly c1 c1
c4=multPoly c2 c2
c6=multPoly c4 c2

mycounter p c n= sum(map (*p!!n) (take (n) c))
x=sum [mycounter p9 c6 n|n<-[0..36]]/4^9/6^6