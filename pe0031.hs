coin z |(z== 1||z==5||z==10||z==25||z==50||z==100||z==200)=1|True=0
--p=[b | a<-[1..200],b<-[-(coin a)]]
lenp=(length p)-1
f 0=[1]
f n= let g=(f (n-1)) in (-(sum (zipWith (*) (take (min (n) (lenp)) (tail p)) g))):g


addPoly p1 p2 = if (length p1 >= length p2)
                then zipWith (+) p1 (p2 ++ repeat 0)
                else addPoly p2 p1
multiplyBy a p1 = map (a*) p1
multiplyByX p = 0:p
multPoly [] p2 = []
multPoly (p:p1) p2 = let pTimesP2 = multiplyBy p p2
                         xTimesP1Timesp2 = multiplyByX $ multPoly p1 p2
                     in addPoly pTimesP2 xTimesP1Timesp2    

penny=1:[if x==1 then -1 else 0|x<-[1..1]]
two=1:[if x==2 then -1 else 0|x<-[1..2]]
nickle=1:[if x==5 then -1 else 0|x<-[1..5]]
dime=1:[if x==10 then -1 else 0|x<-[1..10]]
quarter=1:[if x==20 then -1 else 0|x<-[1..20]]
fifty=1:[if x==50 then -1 else 0|x<-[1..50]]
loonie=1:[if x==100 then -1 else 0|x<-[1..100]]
twonie=1:[if x==200 then -1 else 0|x<-[1..200]]

p= multPoly (multPoly 	(multPoly penny two) 
						(multPoly nickle dime)) 
			(multPoly 	(multPoly quarter fifty) 
						(multPoly loonie twonie))