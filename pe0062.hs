import Data.List
import qualified Data.Map as Map

cubes = [x*x*x | x<-[1..9000]] :: [Int]

explode n 
	|n <10 = [n]
	|otherwise = (mod n 10):(explode (quot n 10))
isPerm m n = (sort (explode m))==(sort (explode n))
implode [] =0
implode (d:lod) = d + 10*(implode lod)

repPerm lod = nub $ permutations lod

n `nthRoot` x = fst $ until (uncurry(==)) (\(_,x0) -> (x0,((n-1)*x0+x/x0**(n-1))/n)) (x,x/n)

is_cube n
	|cr * cr * cr == n = 1
	| otherwise = 0
    where cr = floor $ 3 `nthRoot` (fromIntegral n::Double) 

myCond c = ((sum (map is_cube (filter (myFilter c) (map implode (repPerm (explode c)))))) == 3)
possibleCube7 n =(n `mod` 7 == 0) || (n `mod` 7 == 1) || (n `mod` 7 == 6)
possibleCube13 n =(n `mod` 13 == 0) || (n `mod` 13 == 1) || (n `mod` 13 == 8) || (n `mod` 13 == 12) || (n `mod` 13 == 5)
possibleCube19 n =(n `mod` 19 == 0) || (n `mod` 19 == 1) || (n `mod` 19 == 8) || (n `mod` 19 == 7) || (n `mod` 19 == 11) || (n `mod` 19 == 18) || (n `mod` 19 == 12)
possibleCube61 n =(n `mod` 61 == 0) || (n `mod` 61 == 1) || (n `mod` 61 == 8) || (n `mod` 61 == 27) || (n `mod` 61 == 3) || (n `mod` 61 == 33) || (n `mod` 61 == 38) || (n `mod` 61 == 24) || (n `mod` 61 == 58) || (n `mod` 61 == 50) || (n `mod` 61 == 20) || (n `mod` 61 == 60) || (n `mod` 61 == 9) || (n `mod` 61 == 37) || (n `mod` 61 == 34) || (n `mod` 61 == 28) || (n `mod` 61 == 41) || (n `mod` 61 == 53) || (n `mod` 61 == 23) || (n `mod` 61 == 11) || (n `mod` 61 == 52)
possibleCube67 n =(n `mod` 67 == 0) || (n `mod` 67 == 1) || (n `mod` 67 == 8) || (n `mod` 67 == 27) || (n `mod` 67 == 64) || (n `mod` 67 == 58) || (n `mod` 67 == 15) || (n `mod` 67 == 43) || (n `mod` 67 == 59) || (n `mod` 67 == 62) || (n `mod` 67 == 53) || (n `mod` 67 == 25) || (n `mod` 67 == 9) || (n `mod` 67 == 22) || (n `mod` 67 == 3) || (n `mod` 67 == 40) || (n `mod` 67 == 14) || (n `mod` 67 == 52) || (n `mod` 67 == 66) || (n `mod` 67 == 5) || (n `mod` 67 == 42) || (n `mod` 67 == 24) || (n `mod` 67 == 45)
possibleCube73 n =(n `mod` 73 == 0) || (n `mod` 73 == 1) || (n `mod` 73 == 8) || (n `mod` 73 == 27) || (n `mod` 73 == 64) || (n `mod` 73 == 52) || (n `mod` 73 == 70) || (n `mod` 73 == 51) || (n `mod` 73 == 72) || (n `mod` 73 == 17) || (n `mod` 73 == 49) || (n `mod` 73 == 7) || (n `mod` 73 == 43) || (n `mod` 73 == 22) || (n `mod` 73 == 65) || (n `mod` 73 == 63) || (n `mod` 73 == 3) || (n `mod` 73 == 56) || (n `mod` 73 == 46) || (n `mod` 73 == 21) || (n `mod` 73 == 30) || (n `mod` 73 == 24) || (n `mod` 73 == 9) || (n `mod` 73 == 66) || (n `mod` 73 == 10)
possibleCube79 n =(n `mod` 79 == 0) || (n `mod` 79 == 1) || (n `mod` 79 == 8) || (n `mod` 79 == 27) || (n `mod` 79 == 64) || (n `mod` 79 == 46) || (n `mod` 79 == 58) || (n `mod` 79 == 38) || (n `mod` 79 == 18) || (n `mod` 79 == 52) || (n `mod` 79 == 67) || (n `mod` 79 == 69) || (n `mod` 79 == 57) || (n `mod` 79 == 15) || (n `mod` 79 == 65) || (n `mod` 79 == 21) || (n `mod` 79 == 62) || (n `mod` 79 == 78) || (n `mod` 79 == 12) || (n `mod` 79 == 61) || (n `mod` 79 == 71) || (n `mod` 79 == 41) || (n `mod` 79 == 14) || (n `mod` 79 == 10) || (n `mod` 79 == 33) || (n `mod` 79 == 22) || (n `mod` 79 == 17)
possibleCube97 n =(n `mod` 97 == 0) || (n `mod` 97 == 1) || (n `mod` 97 == 8) || (n `mod` 97 == 27) || (n `mod` 97 == 64) || (n `mod` 97 == 28) || (n `mod` 97 == 22) || (n `mod` 97 == 52) || (n `mod` 97 == 50) || (n `mod` 97 == 30) || (n `mod` 97 == 70) || (n `mod` 97 == 79) || (n `mod` 97 == 63) || (n `mod` 97 == 77) || (n `mod` 97 == 12) || (n `mod` 97 == 69) || (n `mod` 97 == 46) || (n `mod` 97 == 75) || (n `mod` 97 == 42) || (n `mod` 97 == 19) || (n `mod` 97 == 89) || (n `mod` 97 == 34) || (n `mod` 97 == 47) || (n `mod` 97 == 96) || (n `mod` 97 == 67) || (n `mod` 97 == 51) || (n `mod` 97 == 18) || (n `mod` 97 == 45) || (n `mod` 97 == 33)
possibleCubeJN n = (possibleCube7 n)&&(possibleCube13 n)&&(possibleCube19 n)&&(possibleCube61 n)&&(possibleCube67 n)&&(possibleCube73 n)&&(possibleCube79 n)&&(possibleCube97 n)

myFilter c n = (n>=c)&&(possibleCubeJN n)

isPerm m n = (sort (explode m))==(sort (explode n))

myDigs n = (sort (explode n))
myDigs2 n = (length x,x) 
	where x=myDigs n

mylength x = (length x,x)
myFilter2 x = fst x ==5
mydigbox =  filter myFilter2 $sort $  map mylength $group$ sort $ map myDigs2 cubes

mytarget = (snd ((snd (mydigbox!!0))!!0))
myperms =filter (isPerm (implode mytarget)) cubes

--

--c=(345*345*345)
--p=(explode c)
x = head $ filter myCond cubes
--q = (repPerm p)
--r =  (map implode q)
--s= (map is_cube (filter (myFilter c) r))
--s= (map is_cube r)
--s=length r

--w = is_cube c
main = do
		--print x
		--print p
		--print q
		--print r
		--print s
		--print y
		print myperms
