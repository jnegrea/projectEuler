digsum n
	|(0<= n)&&(n<=9) = n
	|otherwise = (n `rem` 10) + (digsum (n `quot` 10))
digsumpair (n,a,b) = (digsum n,a,b)
main :: IO()
main = do
    let ans = (show $ maximum (map digsumpair [(a^b,a,b)|a<-[1..100], b<-[1..100]]))
    print ans