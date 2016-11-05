import Data.List
addMod10to10 x y = mod ((mod x (10^10))+(mod y (10^10))) (10^10)
powerMod10to10 x n
	|n==1 = (mod x (10^10))
	|(mod n 2)==0 = mod ( (powerMod10to10 x (div n 2))^2) (10^10)
	|True = (mod (( (powerMod10to10 x (div n 2))^2)*x) (10^10))


z=foldl addMod10to10 0 (map (\x->powerMod10to10 x x) (filter (\x-> (mod x 10)/=0) [1..1000]))