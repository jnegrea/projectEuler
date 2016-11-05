import Data.List
x=[[75,64,82,10,65,34,67,92,33,29,14,57,48,31,23],
	[95,47,87,47,3,63,70,70,94,51,17,29,40,4,0],
	[17,35,82,75,7,16,80,16,97,68,27,87,60,0,0],
	[18,4,23,73,6,40,37,52,39,50,69,53,0,0,0],
	[20,1,77,28,83,32,91,78,58,16,38,0,0,0,0],
	[19,2,4,56,47,43,17,43,73,93,0,0,0,0,0],
	[88,65,26,33,25,73,91,30,73,0,0,0,0,0,0],
	[99,41,72,65,77,14,67,98,0,0,0,0,0,0,0],
	[41,48,44,28,17,53,70,0,0,0,0,0,0,0,0],
	[41,71,33,38,89,9,0,0,0,0,0,0,0,0,0],
	[53,11,52,68,23,0,0,0,0,0,0,0,0,0,0],
	[70,71,4,27,0,0,0,0,0,0,0,0,0,0,0],
	[91,66,98,0,0,0,0,0,0,0,0,0,0,0,0],
	[63,62,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[4,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]:: [[Int]]

s= (foldl (\y z-> y+ (sum z)) 0 x)
mapfn y 
	|y==0 =0
	|True=(s-y)
	
x2= (map (map mapfn) x )


y1=[[3,4,6,3],[7,4,9,0],[2,5,0,0],[8,0,0,0]]:: [[Int]]
sy=(foldl (\y z-> y+ (sum z)) 0 y1)
mapfny y |y==0 =0|True=(sy-y)
y2= (map (map mapfny) y1 )
pe83 x sx
	|(x!!0!!1+x!!0!!0<=x!!1!!0+x!!0!!0)
		= pe83_recurser (	x,sx,	
							length(x),
							[[(0,1),(0,0)],[(1,0),(0,0)]],
							[x!!0!!1+x!!0!!0,x!!1!!0+x!!0!!0],
							[(0,0),(0,1),(1,0)],
							[]
						)
	|otherwise
		= pe83_recurser (	x,sx,
							length(x),
							[[(1,0),(0,0)],[(0,1),(0,0)]],
							[x!!1!!0+x!!0!!0,x!!0!!1+x!!0!!0],
							[(0,0),(0,1),(1,0)],
							[]
						)

pe83_recurser (x,sx,m,[],[],nodelist,endlist)= (sx*m)-minimum (endlist)

pe83_recurser (x,sx,m,[((l,r):path)],[cost],nodelist,endlist)
	|(l+r)==(m-1) = pe83_recurser (x,sx,m,[],[],nodelist,cost:endlist)
	|(p1 && (not p3))= 	pe83_recurser (x,sx,m,
						snd 
							(unzip 
								( sort 
									(zip 	[((cost+x!!(l+1)!!r)) ]
											[(((l+1,r):(l,r):path))] 
									)
								)
							),
						sort [((cost+x!!(l+1)!!r))],
						(l+1,r):nodelist,endlist)
	|((not p1) && p3 )= pe83_recurser (x,sx,m,
						snd 
							(unzip 
								(sort 
									(zip 	[((cost+x!!l!!(r+1)) )]
											[(((l,r+1):(l,r):path))] 
									)
								)
							),
						sort [((cost+x!!l!!(r+1)))],
						(l,r+1):nodelist,endlist)
	|(p1 && p3 )= pe83_recurser (x,sx,m,
					snd 
						(unzip 
							( sort 
								(zip 	[((cost+x!!(l+1)!!r),(cost+x!!l!!(r+1)) )]
										[((l+1,r):(l,r):path),((l,r+1):(l,r):path)]
								)
							)
						),
					sort [(cost+x!!(l+1)!!r),(cost+x!!l!!(r+1))],
					(l+1,r):(l,r+1):nodelist,endlist) 
	|otherwise = pe83_recurser (x,sx,m,
				[],[],nodelist,endlist)
	where 	
			p1=((l < m-1) && (not (elem (l+1,r+0) nodelist)))
			p3=((r < m-1) && (not (elem (l,r+1) nodelist)))

pe83_recurser (x,sx,m,(((l,r):path):pathlist),(cost:costlist),nodelist,endlist)
	|pathlist==[] = (sx*m)-minimum (endlist)
	|(l+r)==(m-1) = pe83_recurser (x,sx,m,(pathlist),(costlist),nodelist,cost:endlist)
	|(p1 && (not p3))= 	pe83_recurser (x,sx,m,
						snd 
							(unzip 
								( sort 
									(zip 	((cost+x!!(l+1)!!r):costlist) 
											(((l+1,r):(l,r):path):pathlist) )
								)
							),
						sort ((cost+x!!(l+1)!!r):costlist),
						(l+1,r):nodelist,endlist)
	|((not p1) && p3 )= pe83_recurser (x,sx,m,
						snd 
							(unzip 
								( sort 
									(zip 	((cost+x!!l!!(r+1)):costlist )
											(((l,r+1):(l,r):path):pathlist) 
									)
								)
							),
						sort ((cost+x!!l!!(r+1)):costlist),
						(l,r+1):nodelist,endlist)
	|(p1 && p3 )= pe83_recurser (x,sx,m,
					snd 
						(unzip 
							( sort 
								(zip 	((cost+x!!(l+1)!!r):(cost+x!!l!!(r+1)):costlist )
										(((l+1,r):(l,r):path):((l,r+1):(l,r):path):pathlist )
								)
							)
						),
					sort ((cost+x!!(l+1)!!r):(cost+x!!l!!(r+1)):costlist),
					(l+1,r):(l,r+1):nodelist,endlist) 
	|otherwise = pe83_recurser (x,sx,m,
				pathlist,costlist,nodelist,endlist)
	where 	
			p1=((l < m-1) && (not (elem (l+1,r+0) nodelist)))
			p3=((r < m-1) && (not (elem (l,r+1) nodelist)))

