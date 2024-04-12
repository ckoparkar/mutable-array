{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes  #-}

module Data.Array.Mutable.Sort.Merge ( sortInplace ) where

import           Data.Unrestricted.Linear ( Ur(..) )
import qualified Unsafe.Linear as Unsafe

import qualified Data.Array.Mutable.Primitive as A
import qualified Data.Array.Mutable.Prelude as A

--------------------------------------------------------------------------------

sortInplace :: (Ord a, A.Elt a) => A.Array a %1-> A.Array a
sortInplace = Unsafe.toLinear go
  where
    go src =
      let (Ur n, src1) = A.size src
      in writeSort1 src1 (A.makeNoFill n)

writeSort1, writeSort2 :: (Ord a, A.Elt a) => A.Array a -> A.Array a -> A.Array a
writeSort1 src tmp =
  let (Ur !n, src1) = A.size src in
    if n == 1 then src1 else
      let (src_l, src_r) = A.splitMid src
          (tmp_l, tmp_r) = A.splitMid tmp
          tmp_l1 = writeSort2 src_l tmp_l
          tmp_r1 = writeSort2 src_r tmp_r
      in writeMerge tmp_l1 tmp_r1 (A.join src_l src_r)

writeSort2 src tmp =
  let (Ur !n, src1) = A.size src in
    if n == 1 then A.copyOneAndGetDst (src1, tmp) 0 0 else
      let (src_l, src_r) = A.splitMid src
          (tmp_l, tmp_r) = A.splitMid tmp
          src_l1 = writeSort1 src_l tmp_l
          src_r1 = writeSort1 src_r tmp_r
      in writeMerge src_l1 src_r1 (A.join tmp_l tmp_r)

writeMerge :: (Ord a, A.Elt a) => A.Array a -> A.Array a -> A.Array a -> A.Array a
writeMerge left0 right0 = go 0 0 0
  where
    (Ur !nl, left) = A.size left0
    (Ur !nr, right) = A.size right0

    go !il !ir !j dst
      | il == nl =
        A.copyAndGetDst (right, dst) ir j (nr-ir)
      | ir == nr =
        A.copyAndGetDst (left, dst) il j (nl-il)
      | otherwise =
        let (Ur xl, _) = A.unsafeGet left il
            (Ur xr, _) = A.unsafeGet right ir
        in if xl `compare` xr == LT
           then go (il+1) ir (j+1) (A.unsafeSet dst j xl)
           else go il (ir+1) (j+1) (A.unsafeSet dst j xr)
