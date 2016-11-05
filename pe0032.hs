import Data.List
intToList 0 = [];
intToList n = (mod n 10):(intToList (div (n-(mod n 10)) 10))
x=sum $ nub [a*b|a<-[1..99],b<-[1..(div 9999 a)], let (lista,listb,listab)= ((intToList a),(intToList b),(intToList (a*b))) in ((length lista)+(length listb)+(length listab)==9) && (sort ((lista ++listb ++ listab)))==[1..9]]