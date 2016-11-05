import Data.List
isqrt :: Int -> Int
isqrt x = floor . sqrt $ (fromIntegral x :: Float)

evalQuadratic x = (div (-1+isqrt(1+4*x)) 2)
list1 = [((div (m*(m+1)*n*(n+1)) 4),m,n)| m<-[1..2000], n<-[(max 1 ((evalQuadratic (div (2*2000*2000) (m*(m+1))))))..(min m (1+(evalQuadratic (div (2*2000*2001) (m*(m+1))))))], n<=m]

fun (x,y,z) = ((x-2000000)*(x-2000000),x,y,z)
list2 = sort $ map fun list1

fun2 (x,y,z,w)=z*w
x = fun2 $ head list2

main=do 
		let ans = show $ x
		print ans