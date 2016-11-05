import Data.List
import Data.List.Split
pe83 x =pe83_recurser (x,length(x), length(head x),1,[[((m,0))] | m<-[0..(length(x) -1)]],[x!!m!!0 | m<-[0..(length(x) -1)]],[(m,0) | m<-[0..(length(x) -1)]])

	-- |(x!!0!!1+x!!0!!0<=x!!1!!0+x!!0!!0)
	--	= pe83_recurser (	x	,	
	--						length(x),
	--						length(head x),
	--						1,
	--						[[(0,1),(0,0)],[(1,0),(0,0)]],
	--						[x!!0!!1+x!!0!!0,x!!1!!0+x!!0!!0],
	--						[(0,0),(0,1),(1,0)]
	--					)
	-- |otherwise
	--	= pe83_recurser (	x	,	
	--						length(x),
	--						length(head x),
	--						1,
	--						[[(1,0),(0,0)],[(0,1),(0,0)]],
	--						[x!!1!!0+x!!0!!0,x!!0!!1+x!!0!!0],
	--						[(0,0),(0,1),(1,0)]
	--					)


pe83_recurser (x,m,n,maxposn,(((l,r):path):pathlist),(cost:costlist),nodelist)
	|({-l==m-1 && -}r==n-1) =(cost,(l,r):path)
	-- |(p1 && (not p3) && l==m-2 && r==n-1) =(cost+(x!!(m-1)!!(n-1)), (l+1,r):((l,r):path))
	-- |((not p1) && p3 && l==m-1 && r==n-2) =(cost+(x!!(m-1)!!(n-1)), (l,r+1):((l,r):path))
	|(p1 && (not p2) && (not p3) && (not p4) )= 
		pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
						snd 
							(unzip 
								( sort 
									(zip 	((cost+x!!(l+1)!!r):costlist) 
											(((l+1,r):(l,r):path):pathlist) )
								)
							),
						sort ((cost+x!!(l+1)!!r):costlist),
						(l+1,r):nodelist)
	|((not p1) && p2 && (not p3) && (not p4)) = 
		pe83_recurser (x,m,n,maxposn, 
						snd 
							(unzip 
								( sort 
									(zip 	((cost+x!!(l-1)!!r):costlist) 
											(((l-1,r):(l,r):path):pathlist) 
									)
								)
							),
						sort ((cost+x!!(l-1)!!r):costlist),
						(l-1,r):nodelist)
	|((not p1) && (not p2) && p3 && (not p4) )= 
			pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
						snd 
							(unzip 
								( sort 
									(zip 	((cost+x!!l!!(r+1)):costlist )
											(((l,r+1):(l,r):path):pathlist) 
									)
								)
							),
						sort ((cost+x!!l!!(r+1)):costlist),
						(l,r+1):nodelist)
	|((not p1) && (not p2) && (not p3) && p4) =
			pe83_recurser (x,m,n,maxposn, 
						snd 
							(unzip 
								( sort 
									(zip 	((cost+x!!l!!(r-1)):costlist) 
											(((l,r-1):(l,r):path):pathlist )
									)
								)
							),
						sort ((cost+x!!l!!(r-1)):costlist),
						(l,r-1):nodelist)
	|(p1 && p2 && (not p3) && (not p4) )=
			pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
						snd 
							(unzip 
								( sort 
									(zip 	((cost+x!!(l+1)!!r):(cost+x!!(l-1)!!r):costlist)
											(((l+1,r):(l,r):path):((l-1,r):(l,r):path):pathlist )
									)
								)
							),
						sort ((cost+x!!(l+1)!!r):(cost+x!!(l-1)!!r):costlist),
						(l+1,r):(l-1,r):nodelist) 	
	|(p1 && (not p2) && p3 && (not p4) )= 
			pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
					snd 
						(unzip 
							( sort 
								(zip 	((cost+x!!(l+1)!!r):(cost+x!!l!!(r+1)):costlist )
										(((l+1,r):(l,r):path):((l,r+1):(l,r):path):pathlist )
								)
							)
						),
					sort ((cost+x!!(l+1)!!r):(cost+x!!l!!(r+1)):costlist),
					(l+1,r):(l,r+1):nodelist) 
	|(p1 && (not p2) && (not p3) && p4) = 
			pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
					snd 
						(unzip 
							( sort 
								(zip 	((cost+x!!(l+1)!!r):(cost+x!!l!!(r-1)):costlist )
										(((l+1,r):(l,r):path):((l,r-1):(l,r):path):pathlist) 
								)
							)
						),
					sort ((cost+x!!(l+1)!!r):(cost+x!!l!!(r-1)):costlist),
					(l+1,r):(l,r-1):nodelist) 
	|((not p1) && p2 && p3 && (not p4) )= 
			pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
					snd 
						(unzip 
							( sort 
								(zip 	((cost+x!!l!!(r+1)):(cost+x!!(l-1)!!r):costlist )
										(((l,r+1):(l,r):path):((l-1,r):(l,r):path):pathlist )
								)
							)
						),
					sort ((cost+x!!l!!(r+1)):(cost+x!!(l-1)!!r):costlist),
					(l-1,r):(l,r+1):nodelist) 
	|((not p1) && p2 && (not p3) && p4 )= 
			pe83_recurser (x,m,n,maxposn, 
				snd 
					(unzip 
						( sort 
							(zip 	((cost+x!!(l-1)!!r):(cost+x!!l!!(r-1)):costlist )
									(((l-1,r):(l,r):path):((l,r-1):(l,r):path):pathlist )
							)
						)
					),
				sort ((cost+x!!(l-1)!!r):(cost+x!!l!!(r-1)):costlist),
				(l-1,r):(l,r-1):nodelist) 
	|((not p1) && (not p2) && p3 && p4) = 
			pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
				snd 
					(unzip 
						( sort 
							(zip 	((cost+x!!l!!(r+1)):(cost+x!!l!!(r-1)):costlist )
									(((l,r+1):(l,r):path):((l,r-1):(l,r):path):pathlist )
							)
						)
					),
				sort ((cost+x!!l!!(r+1)):(cost+x!!l!!(r-1)):costlist),
				(l,r+1):(l,r-1):nodelist) 
	|(p1 && p2 && p3 && (not p4)) =
			pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
				snd 
					(unzip 
						( sort 
							(zip 	((cost+x!!(l+1)!!r):(cost+x!!l!!(r+1)):(cost+x!!(l-1)!!r):costlist)
									(((l+1,r):(l,r):path):((l,r+1):(l,r):path):((l-1,r):(l,r):path):pathlist )
							)
						)
					),
				sort ((cost+x!!(l+1)!!r):(cost+x!!l!!(r+1)):(cost+x!!(l-1)!!r):costlist),
				(l+1,r):(l-1,r):(l,r+1):nodelist) 		 
	|(p1 && p2 && (not p3) && p4 )= 
			pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
				snd 
					(unzip 
						( sort 
							(zip 	((cost+x!!(l+1)!!r):(cost+x!!(l-1)!!r):(cost+x!!l!!(r-1)):costlist)
									(((l+1,r):(l,r):path):((l-1,r):(l,r):path):((l,r-1):(l,r):path):pathlist )
							)
						)
					),
				sort ((cost+x!!(l+1)!!r):(cost+x!!(l-1)!!r):(cost+x!!l!!(r-1)):costlist),
				(l+1,r):(l-1,r):(l,r-1):nodelist) 	
	|(p1 && (not p2) && p3 && p4 )= 
			pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
				snd 
					(unzip 
						( sort 
							(zip 	((cost+x!!(l+1)!!r):(cost+x!!l!!(r+1)):(cost+x!!l!!(r-1)):costlist)
									(((l+1,r):(l,r):path):((l,r+1):(l,r):path):((l,r-1):(l,r):path):pathlist )
							)
						)
					),
				sort ((cost+x!!(l+1)!!r):(cost+x!!l!!(r+1)):(cost+x!!l!!(r-1)):costlist),
				(l+1,r):(l,r-1):(l,r+1):nodelist) 		 
	|((not p1) && p2 && p3 && p4 )=
			pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
				snd 
					(unzip 
						( sort 
							(zip 	((cost+x!!l!!(r+1)):(cost+x!!(l-1)!!r):(cost+x!!l!!(r-1)):costlist)
									(((l,r+1):(l,r):path):((l-1,r):(l,r):path):((l,r-1):(l,r):path):pathlist )
							)
						)
					),
				sort ((cost+x!!l!!(r+1)):(cost+x!!(l-1)!!r):(cost+x!!l!!(r-1)):costlist),
				(l-1,r):(l,r-1):(l,r+1):nodelist)
	|otherwise =
		pe83_recurser (x,m,n,maximum [maxposn,l+r+1], 
				pathlist,costlist,nodelist)
	where 	
			p1=((l < m-1) && (not (elem (l+1,r+0) nodelist)))
			p2=((l > 0) && (not (elem (l-1,r+0) nodelist)))
			--p2=False
			p3=((r < n-1) && (not (elem (l,r+1) nodelist)))
			--p4=((r > 0) && (not (elem (l,r-1) nodelist)))
			p4 = False

--test = [[131,673,234,103,18],[201,96,342,965,150],[630,803,746,422,111],[537,699,497,121,956],[805,732,524,37,331]]

--main =do
--	print (test!!0!!4)
 --Create a matrix parsing function
readMat :: String -> [[Int]]
readMat = (map (map (read :: String -> Int)) . map (splitOn "," )) . words
main :: IO()
main = do
        rawMatrix <- readFile "p083_matrix.txt"
        let parsedMatrix = readMat rawMatrix
        --let parsedMatrix = test
        let ans = pe83 parsedMatrix
        writeFile "pe83.txt" $ show ans
        print ans
