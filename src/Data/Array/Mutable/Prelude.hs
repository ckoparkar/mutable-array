{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Mutable.Prelude where

import           Prelude hiding ( last, splitAt, sum, foldl )
import           Data.Unrestricted.Linear ( Ur(..), lseq)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude.Linear as Linear
import           Data.Array.Mutable.Primitive

--------------------------------------------------------------------------------

{-# INLINE last #-}
last :: Elt a => Array a %1-> Ur a
last = Unsafe.toLinear go
  where
    go arr0 =
      let (Ur n, arr1) = size arr0
          (x, arr2) = unsafeGet arr1 (n-1)
      in arr2 `lseq` x

{-# INLINE unsafeSwap #-}
swap :: Elt a => Array a %1-> Int -> Int -> Array a
swap = Unsafe.toLinear go
  where
    go xs i j =
      let (Ur !xi, xs1) = get xs i
          (Ur xj, xs2) = get xs1 j
          xs3 = set xs2 i xj
          xs4 = set xs3 j xi
      in xs4

{-# INLINE swap #-}
unsafeSwap :: Elt a => Array a %1-> Int -> Int -> Array a
unsafeSwap = Unsafe.toLinear go
  where
    go xs i j =
      let (Ur !xi, xs1) = unsafeGet xs i
          (Ur xj, xs2) = unsafeGet xs1 j
          xs3 = unsafeSet xs2 i xj
          xs4 = unsafeSet xs3 j xi
      in xs4

{-# INLINE splitAt #-}
splitAt :: Elt a => Array a %1-> Int -> (Array a, Array a)
splitAt = Unsafe.toLinear go
  where
    go xs m =
      let (Ur n, xs1) = size xs
          (s1, xs2) = slice xs1 0 m
          (s2, xs3) = slice xs2 m (n-m)
      in xs3 `lseq` (s1, s2)

{-# INLINE splitMid #-}
splitMid :: Elt a => Array a %1-> (Array a, Array a)
splitMid = Unsafe.toLinear go
  where
    go xs =
      let (Ur n, xs1) = size xs
          h = n `div` 2
      in splitAt xs1 h

{-# INLINE copyAndGetDst #-}
copyAndGetDst :: Elt a => (Array a, Array a) %1-> Int -> Int -> Int -> Array a
copyAndGetDst (src,dst) i j n =
  copy (src,dst) i j n Linear.&
  \(src1,dst1) -> src1 `lseq` dst1

{-# INLINE copyOneAndGetDst #-}
-- This might be more efficient that calling copy, not verified by a benchmark.
copyOneAndGetDst :: Elt a => (Array a, Array a) %1-> Int -> Int -> Array a
copyOneAndGetDst (src,dst) i j =
  unsafeGet src i Linear.&
  \(Ur x, src1) -> src1 `lseq` unsafeSet dst j x

--------------------------------------------------------------------------------

{-# INLINE sum #-}
sum :: (Num a, Elt a) => Array a %1-> Ur a
sum a0 = size a0 Linear.& \(Ur !n, a1) -> (go a1 0 n 0)
  where
    go :: (Num a, Elt a) => Array a %1 -> Int -> Int -> a -> Ur a
    go xs0 !i n !acc
      | i == n    = xs0 `lseq` Ur acc
      | otherwise = unsafeGet xs0 i Linear.&
                    \(Ur !xi, xs1) -> go xs1 (i+1) n (acc+xi)

{-# INLINE generate #-}
generate :: (Num a, Elt a) => Int -> (Int -> a) -> (Array a %1-> Ur b) %1-> Ur b
generate n g f = f (generate' m 0 g (makeNoFill m))
  where
    m = n `max` 0

{-# INLINE generate' #-}
generate' :: (Num a, Elt a) => Int -> Int -> (Int -> a) -> Array a -> Array a
generate' !m !off g = go 0
  where
    go !i arr
      | i == m    = arr
      | otherwise = go (i+1) (unsafeSet arr i (g (i+off)))

{-# INLINE foldl #-}
foldl :: forall a b. Elt a => (b -> a %1-> b) -> b %1-> Array a %1-> Ur b
foldl f = Unsafe.toLinear2 go2
  where
    go2 acc arr0 =
      let (Ur n, arr1) = size arr0
      in Ur (go arr1 acc 0 n)
    go arr0 acc0 !i !n
      | i == n    = acc0
      | otherwise = let (Ur xi, arr1) = unsafeGet arr0 i
                        acc1 = f acc0 xi
                    in go arr1 acc1 (i+1) n
