import Data.List
intToList 0 = [];
intToList n = (mod n 10):(intToList (div (n-(mod n 10)) 10))
intToListBin 0 = [];
intToListBin n = (mod n 2):(intToListBin (div (n-(mod n 2)) 2))

palindromes=[x | x<-[1..(10^6)],let lix=(intToList x) in lix==(reverse lix),let blix=(intToListBin x) in blix==(reverse blix)]

