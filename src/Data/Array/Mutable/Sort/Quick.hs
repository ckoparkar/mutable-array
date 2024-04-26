{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes  #-}

module Data.Array.Mutable.Sort.Quick ( sort, sortInplace, sort', sortInplace' ) where

import           Data.Unrestricted.Linear ( Ur(..), lseq )
import qualified Unsafe.Linear as Unsafe

import qualified Data.Array.Mutable.Sort.Insertion as Insertion
import qualified Data.Array.Mutable.Primitive as A
import qualified Data.Array.Mutable.Prelude as A

--------------------------------------------------------------------------------

{-# INLINE sort #-}
sort, sortInplace :: (Ord a, A.Elt a) => A.Array a %1-> A.Array a
sort = Unsafe.toLinear go
  where
    go src =
      let (Ur n, src1) = A.size src
          (src2, dst)  = A.copy (src1, A.makeNoFill n) 0 0 n
      in src2 `lseq` sortInplace dst

sortInplace = Unsafe.toLinear go
  where
    go arr =
      let (Ur n, arr1) = A.size arr
      in qsort 0 (n-1) arr1

    qsort !lo !hi arr
      | (hi-lo) <= 0 = arr
      | otherwise    = let (pivot, arr1) = partition lo hi arr
                       in qsort (pivot+1) hi $
                          qsort lo (pivot-1) $
                          arr1

sort', sortInplace' :: (Ord a, A.Elt a) => A.Array a %1-> A.Array a
sort' = Unsafe.toLinear go
  where
    go src =
      let (Ur n, src1) = A.size src
          (src2, dst)  = A.copy (src1, A.makeNoFill n) 0 0 n
      in src2 `lseq` sortInplace' dst

sortInplace' = Unsafe.toLinear go
  where
    go arr =
      let (Ur n, arr1) = A.size arr
      in qsort 0 (n-1) arr1

    qsort !lo !hi arr
      -- | (hi-lo) <= 0  = arr
      | (hi-lo) <= 20 = let (tosort, orig) = A.slice arr lo (hi-lo+1)
                        in (Insertion.sortInplace tosort) `lseq` orig
      | otherwise     = let (pivot, arr1) = partition lo hi arr
                        in qsort (pivot+1) hi $
                           qsort lo (pivot-1) $
                           arr1

partition :: (Ord a, A.Elt a) => Int -> Int -> A.Array a -> (Int, A.Array a)
partition lo hi = go
  where
    go arr = let (Ur !xp, arr1) = A.unsafeGet (setupPivot arr) hi
                 (!pivot, arr2) = loop xp lo lo arr1
             in (pivot, A.unsafeSwap arr2 pivot hi)

    loop !xp !i !j arr
      | i >= hi   = (j, arr)
      | otherwise = let (Ur !xi, arr1) = A.unsafeGet arr i
                    in if xi < xp
                       then loop xp (i+1) (j+1) (A.unsafeSwap arr1 i j)
                       else loop xp (i+1) j arr1

    setupPivot arr
      -- | hi-lo > 3 = middleElt arr
      | hi-lo > 3 = medianOf3 arr
      | otherwise = arr

    middleElt arr =
      A.unsafeSwap arr ((lo+hi) `div` 2) hi

    medianOf3 arr =
      let mid = (lo+hi) `div` 2
          (Ur xlo, arr1)  = A.unsafeGet arr lo
          (Ur xmid, arr2) = A.unsafeGet arr1 mid
          (Ur xhi, arr3)  = A.unsafeGet arr2 hi
      in if (xmid > xlo) && (xmid < xhi)
         then A.unsafeSwap arr3 mid hi
         else if (xlo > xmid) && (xmid < xhi)
              then A.unsafeSwap arr3 lo hi
              else arr3
