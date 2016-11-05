
import Data.List
import Data.Array.Base
  ( UArray
  , castSTUArray
  )
import Data.Array.IArray
  ( IArray
  , bounds
  , (!)
  )
import Data.Array.MArray
import GHC.Base
import GHC.Integer
import GHC.Integer.GMP.Internals

import Data.Array.Unboxed
import Data.Array.ST

import Data.Bits


squareRoot :: Int -> Int
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^2 <= n && n < (r+1)^2
  in  head $ dropWhile (not . isRoot) iters

isPossibleSquare :: Int -> Bool
isPossibleSquare n =
  unsafeAt sr256 ((fromIntegral n) .&. 255)
  && unsafeAt sr693 (fromIntegral (n `rem` 693))
  && unsafeAt sr325 (fromIntegral (n `rem` 325))


unsafeAt :: (IArray a e, Ix i) => a i e -> i -> e
unsafeAt = (!)

unsafeFreeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
unsafeFreeze = freeze

unsafeNewArray_ :: (Ix i, MArray a e m) => (i, i) -> m (a i e)
unsafeNewArray_ = newArray_

unsafeRead :: (MArray a e m, Ix i) => a i e -> i -> m e
unsafeRead = readArray

unsafeThaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
unsafeThaw = thaw

unsafeWrite :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
unsafeWrite = writeArray

sqRemArray :: Int -> UArray Int Bool
sqRemArray md = runSTUArray $ do
  arr <- newArray (0,md-1) False
  let stop = (md `quot` 2) + 1
      fill k
        | k < stop  = unsafeWrite arr ((k*k) `rem` md) True >> fill (k+1)
        | otherwise = return arr
  unsafeWrite arr 0 True
  unsafeWrite arr 1 True
  fill 2

sr256 :: UArray Int Bool
sr256 = sqRemArray 256

sr819 :: UArray Int Bool
sr819 = sqRemArray 819

sr4097 :: UArray Int Bool
sr4097 = sqRemArray 4097

sr341 :: UArray Int Bool
sr341 = sqRemArray 341

sr1025 :: UArray Int Bool
sr1025 = sqRemArray 1025

sr2047 :: UArray Int Bool
sr2047 = sqRemArray 2047

sr693 :: UArray Int Bool
sr693 = sqRemArray 693

sr325 :: UArray Int Bool
sr325 = sqRemArray 325

isSquare' :: Int -> Bool
isSquare' n = isPossibleSquare n && let r = squareRoot n in r*r == n

isPyth:: (Int,Int)->Bool
isPyth (x,y) = isSquare' (x*x + y*y)

countRect:: Int -> (Int,Int)
countRect m = (length [(a,b)|b<-[1..m], a<-[1..b], (isPyth (a+b,m))],m)

list1=map countRect [1..]

adder (a,m) (b,n) = (a+b,n)
list2=filter (>=(2000,0)) $ scanl adder (0,0) list1

--x = head [m | m<-[1..], (countRect m) > 2000]
x= take 1 list2

--x=isSquare' 4

main=do 
		let ans = show $ x
		print ans