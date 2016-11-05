intToList 0 = [];
intToList n = (mod n 10):(intToList (div (n-(mod n 10)) 10))

x=sum [ a| a<-[2..10^6], sum (map (^5) (intToList a))==a] 