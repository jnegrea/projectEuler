
f p =length [(a,b) | a<-[(div p 3) .. (div p 2)], b<-[(max (max 1 (p-3*a)) (div (p-(2*a)) 2))..(min a (p-2*a))], ((p-a-b)^2)==((a^2)+(b^2))]

x=maximum [(f p,p)|p<-[1..1000]]