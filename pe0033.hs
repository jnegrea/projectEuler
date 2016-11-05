--fracEq (an,ad) (bn,bd) = (an*bd==ad*bn)
--simplify (an,ad) = let g=gcd an ad in ((div an g),(div ad g))
--fracAdd (an,ad) (bn,bd)= simplify (an*bd+ad*bn, ad*bd)
--fracMult (an,ad) (bn,bd)=simplify (an*bn,ad*bd)
--fracSub (an,ad) (bn,bd) = fracAdd (an,ad) (-bn,bd)
--fracDiv (an,ad) (bn,bd)=fracMult (an,ad) (bd,bn)



simplify (an,ad) = let g=gcd an ad in ((div an g),(div ad g))
fracMult (an,ad) (bn,bd)=simplify (an*bn,ad*bd)
x=snd $ foldr fracMult (1,1) $ 
	map simplify 
		[(a*10+b,c*10+d) | a<-[1..9], b<-[1..9], c<-[a..9], d<-[1..9],
			let (num,den,simp)=(a*10+b,c*10+d ,simplify (a*10+b,c*10+d))
			in	num<den && (
				(simp == simplify (a, c)) && b==d ||
				(simp == simplify (a, d)) && b==c ||
				(simp == simplify (b, c)) && a==d ||
				(simp == simplify (b, d)) && a==c)] 
