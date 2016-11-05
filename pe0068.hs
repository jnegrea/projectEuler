import Data.List

perms n = permutations [1..n]

pentaRings = sort [[-o1,-i1,-i2,-o2,-i2,-i3,-o3,-i3,-i4,-o4,-i4,-i5,-o5,-i5,-i1]|
	o1<-[1..6],
	o2<-[(o1+1)..10],
	o3<-(filter (/= o2) [(o1+1)..10]),
	o4<-(filter (\x-> (x/=o2)&&(x/=o3)) [(o1+1)..10]),
	o5<-(filter (\x-> (x/=o2)&&(x/=o3)&&(x/=o4)) [(o1+1)..10]),
	i1<-(filter (\x-> (x/=o1)&&(x/=o2)&&(x/=o3)&&(x/=o4)&&(x/=o5)) [1..10]),
	i2<-(filter (\x-> (x/=o1)&&(x/=o2)&&(x/=o3)&&(x/=o4)&&(x/=o5)&&(x/=i1)) [1..10]),
	i3<- [o1-o2+i1],
	i4<- [o2-o3+i2],
	i5<- [o3-o4+i3],
	sort [o1,o2,o3,o4,o5,i1,i2,i3,i4,i5] == [1..10]
	]

x = foldl (\x y->if y<10 then y+10*x else y+100*x ) 0 $ map (\x-> -x) $ head pentaRings


main = do
	print x