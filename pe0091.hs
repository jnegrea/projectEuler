prop ((x1,y1),(x2,y2)) = ((x1^2+y1^2)+(x2^2+y2^2)==(x1-x2)^2+(y1-y2)^2) || ((x1^2+y1^2)+(x1-x2)^2+(y1-y2)^2 ==(x2^2+y2^2)) || ((x2^2+y2^2)+(x1-x2)^2+(y1-y2)^2 ==(x1^2+y1^2))
myDiv a b n
	|b==0 = n
	|(div a b)*b==a = (div a b) -1
	|otherwise = (div a b)
ifX1_0 x1
	|x1 == 0 = 1
	|otherwise  = 0
triangles n = [((x1,y1),(x2,y2))| x1<-[0..n], y1<-[0..n], x2<-[(ifX1_0 x1)..n], y2<-[0..(min n (myDiv (x2*y1) x1 n))], prop ((x1,y1),(x2,y2)), ((x1/=0)||(y1/=0))&&((x2/=0)||(y2/=0)) ]
test = length $ triangles 50


main= do 
	print test