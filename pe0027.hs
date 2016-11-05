pp n (x:[])|x*x>n = True|True = (mod n x /=0)
pp n (x:xs)|x*x>n = True|(mod n x /=0) = pp n xs|True=False
isPrime n = (n>=2)&&(pp n [2..n])
x= (filter isPrime (2:[3,5..999]))
y=maximum [(m,a,b) |a<-[-999..999],b<-x, m<-[length (takeWhile isPrime [n^2+a*n+b|n<-[0..]])],m>=40]