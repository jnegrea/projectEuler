import Data.List  
primes:: [Int]->[Int]
primes []=[]
primes [a]=[a]
primes (a:xs)=a:(primes (filter (\x-> mod x a /=0) xs))

intToList :: Int -> [Int]
intToList =map (read . (:[])) . show

ppp _ []=True
ppp n (x:[])
		|x*x>n = True
		|True = (mod n x /=0)
ppp n (x:xs)
		|x*x>n = True
		|(mod n x /=0) = ppp n xs
		|True=False

isPrime n = ppp n [2..n]
perm1 loi=let s=  (sort ( intToList (loi!!0))) in s==(sort (intToList (loi!!1))) && s==(sort (intToList (loi!!2)))

candidates0=filter (1009<=) (primes (2:[3,5..9973]))
candidates1 = [[y,x,2*x-y] | x<-candidates0, y<-(takeWhile (<x) candidates0),2*x-y <=9973, isPrime (2*x-y)]
candidates2 = filter perm1 candidates1 