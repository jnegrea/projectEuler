import Data.List  
primes:: [Int]->[Int]
primes []=[]
primes [a]=[a]
primes (a:xs)=a: (primes (filter (\x-> mod x a /=0) xs))


plist= primes (2:[3,5..])
nextprime n = head $ filter (>n) plist


psums 1 = zip plist plist
psums n=takeWhile (\x-> (snd x)<1000000) [let p =nextprime(fst ps) in (p,(snd ps)+p) |ps<-(psums (n-1))]


ppp _ []=True
ppp n (x:[])
		|x*x>n = True
		|True = (mod n x /=0)
ppp n (x:xs)
		|x*x>n = True
		|(mod n x /=0) = ppp n xs
		|True=False

isPrime n = ppp n [2..n]

ppsums n = filter (\x-> isPrime (snd x)) (psums n)

pl= [m | m<-[1,3..1000], not (null (ppsums m)) ]