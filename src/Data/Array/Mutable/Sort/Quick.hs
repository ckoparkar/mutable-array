{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes  #-}

module Data.Array.Mutable.Sort.Quick ( sortInplace ) where

import           Data.Unrestricted.Linear ( Ur(..) )
import qualified Unsafe.Linear as Unsafe

import qualified Data.Array.Mutable.Primitive as A
import qualified Data.Array.Mutable.Prelude as A

--------------------------------------------------------------------------------

sortInplace :: (Show a, Ord a, A.Elt a) => A.Array a %1-> A.Array a
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

partition :: (Show a, Ord a, A.Elt a) => Int -> Int -> A.Array a -> (Int, A.Array a)
partition lo hi arr00 =
  let (j, arr2) = go lo lo arr0
      arr3 = A.swap arr2 j hi
  in (j, arr3)
  where
    (Ur !xpivot, arr0) = A.unsafeGet arr00 hi

    go !i !j arr
      | i >= hi   = (j, arr)
      | otherwise = let (Ur xi, arr1) = A.unsafeGet arr i
                    in if xi < xpivot
                       then go (i+1) (j+1) (A.swap arr1 i j)
                       else go (i+1) j arr1
