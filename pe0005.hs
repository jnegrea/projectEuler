gcd' :: Int -> Int -> Int
gcd' 0 n=n
gcd' n 0=n
gcd' 1 n=1
gcd' n 1=1
gcd' n m
	|(n==m) = n
	|(n>m) = gcd' m (mod n m)
	|(m>n) = gcd' n (mod m n)

lcm' :: Int -> Int -> Int
lcm' m n= div (m*n) (gcd' m n)

listlcm (x:(y:xs))
	|(xs==[]) = lcm' x y
	|otherwise = listlcm ((lcm x y):xs)
