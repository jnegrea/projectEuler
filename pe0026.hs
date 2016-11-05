
pp n (x:[])|x*x>n = True|True = (mod n x /=0)
pp n (x:xs)|x*x>n = True|(mod n x /=0) = pp n xs|True=False
isPrime n = pp n [2..n]

makeNines = 9: (map (\x->10*x+9) makeNines)
x= (filter isPrime (2:[3,5..999]))
x1= zip [1..] makeNines
main=maximum $ map (\w->(fst $ head $ filter (\z-> mod (snd z) w ==0) x1,w )) x