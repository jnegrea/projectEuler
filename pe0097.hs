modPower b e n
	|e==0 			= 1
	|e==1 			= mod b n
	|e `mod` 2 == 0 = let x = (modPower b (div e 2) n) in mod (x*x) n
	|otherwise 		= let x = (modPower b (div e 2) n) in mod ((mod (x*x) n)*b) n

ans= mod (28433*(modPower 2 7830457 (10^10))+1) (10^10)
main=do print ans 