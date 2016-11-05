import Data.List

intToList :: Integer -> [Integer]
intToList =map (read . (:[])) . show

listToInteger:: [Integer] -> Integer
listToInteger lon=foldl (\x y -> 10*x+y) 0 lon

xprevx:: Integer -> Integer
xprevx x = x + (listToInteger (reverse (intToList x)))

isPlndrm:: Integer->Bool
isPlndrm x = (intToList x) == (reverse (intToList x))

isLycherelH x n 
	|n==0 = True
	|isPlndrm z = False
	|True = isLycherelH z (n-1)
	where z=(xprevx x)

isLycherel x = isLycherelH x 50

