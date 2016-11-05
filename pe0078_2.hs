--addPoly p1 p2 = if (length p1 >= length p2)
--                then zipWith (+) p1 (p2 ++ repeat 0)
--                else addPoly p2 p1
--multiplyBy a p1 = map (a*) p1
--multiplyByX p = 0:p
--multPoly [] p2 = []
--multPoly (p:p1) p2 = let pTimesP2 = multiplyBy p p2
--                         xTimesP1Timesp2 = multiplyByX $ multPoly p1 p2
--                     in addPoly pTimesP2 xTimesP1Timesp2   


--qm m = [if k==0 then 1 else if k==m then -1 else 0|k<-[0..m]]

--qq 1 = qm 1
--qq m= multPoly (qm m) (qq (m-1))

--q=qq 100----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--f 0=1
--f 1 =(-p!!0)*(q!!1)
--f n= (-1)*(sum$ zipWith (*) (take (n) $ tail q) (reverse $ take (n) p))
--p=[f n|n<-[0..]]

import Data.Array

p:: Int ->Integer
p 0 = 0
p 1 = 1
p n = sum [(fromIntegral ((-1)^(k+1)) ::Integer)*( (fromIntegral (p'!(max 0 (n-(div (k*(3*k-1)) 2))))::Integer)+ (fromIntegral (p'!(max 0 (n-(div (k*(3*k+1)) 2))))::Integer)) | k<-[1..bound n] ]


bound n = ceiling ((1+(sqrt (1+24*(fromIntegral n::Double)))/6))

p'::Array Int Integer
--p'::[Integer]
p'=array (0,2000000) [(n,(p n))|n<-[0..2000000]]

q = [(n,(pn))|n<-[1..2000000], let pn=p'!n, pn `mod` 1000000 ==0]
ans=head q

--ans=take 102 p'

main=do
	print ans
