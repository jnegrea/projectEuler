import Data.List

div17pdn=[[g,h,i]|g<-[0..9], h<-[0..9]\\[g], i<-[0..9]\\[g,h], mod (100*g+10*h+i) 17 ==0]
div13pdn=[[f,g,h]|f<-[0..9], g<-[0..9]\\[f], h<-[0..9]\\[f,g], mod (100*f+10*g+h) 13 ==0]
div11pdn=[[e,f,g]|e<-[0..9], f<-[0..9]\\[e], g<-[0..9]\\[e,f], mod (100*e+10*f+g) 11 ==0]
div7pdn=[[d,e,f]|d<-[0..9], e<-[0..9]\\[d], f<-[0..9]\\[d,e], mod (100*d+10*e+f) 7 ==0]
div5pdn=[[c,d,e]|c<-[0..9], d<-[0..9]\\[c], e<-[0..9]\\[c,d], mod (100*c+10*d+e) 5 ==0]
div3pdn=[[b,c,d]|b<-[0..9], c<-[0..9]\\[b], d<-[0..9]\\[b,c], mod (100*b+10*c+d) 3 ==0]
div2pdn=[[a,b,c]|a<-[0..9], b<-[0..9]\\[a], c<-[0..9]\\[a,b], mod (100*a+10*b+c) 2 ==0]

x=sum [sum (zipWith (*) (map (10^) [9,8..0]) [m, a, b, c, d, e, f, g, h, i]) | 
	[g,h,i]<-div17pdn, 
	[d,e,f]<-filter (\x-> not (x!!0 `elem` [g,h,i] ||x!!1 `elem` [g,h,i] || x!!2 `elem` [g,h,i])) div7pdn, 
	[a,b,c]<-filter (\x-> not (x!!0 `elem` [d,e,f,g,h,i] ||x!!1 `elem` [d,e,f,g,h,i] || x!!2 `elem` [d,e,f,g,h,i])) div2pdn,	
	m<-[0..9]\\[a,b,c,d,e,f,g,h,i],
	[f,g,h] `elem` div13pdn,
	[e,f,g] `elem` div11pdn,
	[c,d,e] `elem` div5pdn,
	[b,c,d] `elem` div3pdn
	]
y=[]