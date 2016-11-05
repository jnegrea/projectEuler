import Data.List
intToList 0 = [];
intToList n = (mod n 10):(intToList (div (n-(mod n 10)) 10))
factorial n = product [2..n]
x=sum [a | a<-[3..999999], sum ( map factorial (intToList a))==a]