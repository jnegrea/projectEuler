
--primes:: [Int]->Int
--primes []=0
--primes [a]=a
--primes (a:xs)=a+(primes (filter (\x-> mod x a /=0) xs))

--main = putStrLn (show ((primes [2..200000])))
ppp _ []=True
ppp n (x:[])
		|x*x>n = True
		|True = (mod n x /=0)
ppp n (x:xs)
		|x*x>n = True
		|(mod n x /=0) = ppp n xs
		|True=False

isPrime n = ppp n [2..n]
main= putStrLn (show (sum (filter isPrime [2..2000000])))