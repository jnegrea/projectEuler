--import Data.List
oddies 1=[1,3..9]
oddies n=[10*e+d|e<-(oddies (n-1)),d<-(oddies 1)]

--reverse :: Int -> [Int]
reverseInt x= (read (reverse (show x)))::Int
sor n = 2*sum (map (\x-> if (n-x)==(reverseInt x) then 1 else 0) [1..(div n 2)])

x=(sum (map sor (oddies 1)))
	+(sum (map sor (oddies 2)))

	

y=1			+(sum (map sor (oddies 3)))
	+(sum (map sor (oddies 4)))+(sum (map sor (oddies 5)))
	+(sum (map sor (oddies 6)))
	+(sum (map sor (oddies 7)))
	+(sum (map sor (oddies 8)))
	+(sum (map sor (oddies 9)))
