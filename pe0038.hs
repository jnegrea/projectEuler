import Data.List
intToList 0 = [];
intToList n = (intToList (div (n-(mod n 10)) 10))++[(mod n 10)]

x=maximum[(foldr (\z y-> (intToList (a*z))++y) [] [1..n])|a<-[1..9999],n<-[1..(div 9 ((length (intToList a))))],(sort (foldr (\z y-> (intToList (a*z))++y) [] [1..n]))==[1..9]]