import Data.List(sort,sortBy)
-- |  The 'nubSort' function is equivalent to @'nub' '.' 'sort'@,  except
-- somewhat more efficient as duplicates are removed as it sorts.  It is
-- essentially Data.List.sort,  with 'merge' replaced by 'union'.
unionBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
unionBy cmp = loop
  where
     loop [] ys = ys
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT -> x : loop xs (y:ys)
          EQ -> x : loop xs ys
          GT -> y : loop (x:xs) ys

nubSort :: Ord a => [a] -> [a]
nubSort = nubSortBy compare

-- |  The 'nubSortBy' function is the non-overloaded version of 'nubSort'.
nubSortBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortBy cmp = foldTree' (unionBy cmp) [] . runs
  where
    -- 'runs' partitions the input into sublists that are monotonic,
    -- contiguous,  and non-overlapping.   Descending runs are reversed
    -- and adjacent duplicates are eliminated,  so every run returned is
    -- strictly ascending.

    runs (a:b:xs)
      = case cmp a b of
          LT -> asc b (a:) xs
          EQ -> runs (a:xs)
          GT -> desc b [a] xs
    runs xs = [xs]

    desc a as []  = [a:as]
    desc a as (b:bs)
      = case cmp a b of
          LT -> (a:as) : runs (b:bs)
          EQ -> desc a as bs
          GT -> desc b (a:as) bs

    asc a as [] = [as [a]]
    asc a as (b:bs)
      = case cmp a b of
         LT -> asc b (\ys -> as (a:ys)) bs
         EQ -> asc a as bs
         GT -> as [a] : runs (b:bs)
ppp n (x:[])
		|x*x>n = n
		|(mod n x /=0) = n
		|True = x
ppp n (x:xs)
		|x*x>n = n
		|(mod n x /=0) = ppp n xs
		|True=x 
foldTree' :: (a -> a -> a) -> a -> [a] -> a
foldTree' plus zero xs
  = case xs of
      []    -> zero
      (_:_) -> loop xs
  where
    loop [x] = x
    loop xs  = loop (pairs xs)

    pairs (x:y:zs) = plus x y : pairs zs
    pairs zs       = zs


divisorMultiplicityH:: Int ->[(Int,Int)]->[(Int,Int)]
divisorMultiplicityH n []
		|n==1 =[]
		|True=divisorMultiplicityH (div n x) [(1,x)]
		where x= ppp n [2..n]
divisorMultiplicityH n ((y1,y2):[])
		|n==1 =((y1,y2):[])
		|(y2 == x) = divisorMultiplicityH (div n x) ((y1+1,y2):[])
		|True= divisorMultiplicityH (div n x) ((1,x):((y1,y2):[]))
		where x= ppp n [2..n]
divisorMultiplicityH n ((y1,y2):ys)
		|n==1 =((y1,y2):ys)
		|(y2 == x) = divisorMultiplicityH (div n x) ((y1+1,y2):ys)
		|True= divisorMultiplicityH (div n x) ((1,x):((y1,y2):ys))
		where x= ppp n [2..n]

divisorMultiplicity:: Int ->[(Int,Int)]
divisorMultiplicity n = divisorMultiplicityH n []

divisorSum n=(product $ map (\(a,b)->(div (b^(a+1) - 1) (b-1))) (divisorMultiplicity n) )

x=[a | a<-[1..28132], (divisorSum a) > (2*a)]
x2= nubSort [a+b |a<-x,b<-x, b<=a]

myfind a (x:slon)
	|x==a = True
	|x>a = False
	|slon==[] =False
	|True=myfind a slon

summer:: [Int] -> [Int] -> Int
summer (i:ilist) (a:alist)
	|(ilist==[]||alist==[])=0
	|(i<a) = i+(summer ilist (a:alist))
	|(i==a)= (summer ilist alist)

