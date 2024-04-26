{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes  #-}

module Data.Array.Mutable.Sort.Cilk
  ( sort, sortPar, sortPar4, sortParM, sortParM4
  , sortInplace, sortInplacePar, sortInplacePar4, sortInplaceParM, sortInplaceParM4 ) where

import           GHC.Conc ( par, pseq )
import           Data.Unrestricted.Linear ( Ur(..), lseq )
import qualified Unsafe.Linear as Unsafe
import qualified Control.Monad.Par as Par (Par, spawn_, get)

import qualified Data.Array.Mutable.Primitive as A
import qualified Data.Array.Mutable.Parallel as P
import qualified Data.Array.Mutable.Sort.Insertion as Insertion
import qualified Data.Array.Mutable.Sort.Quick as Quick
import qualified Data.Array.Mutable.Prelude as A

--------------------------------------------------------------------------------

sort, sortPar, sortPar4, sortInplace, sortInplacePar, sortInplacePar4 ::
  (Ord a, A.Elt a) => A.Array a %1-> A.Array a
sort            = sort' A.copy    sortInplace
sortPar         = sort' P.copyPar sortInplacePar
sortPar4        = sort' P.copyPar sortInplacePar4
sortInplace     = sortInplace' writeSort1
sortInplacePar  = sortInplace' writeSort1Par
sortInplacePar4 = sortInplace' writeSortPar4

sort' :: (Ord a, A.Elt a)
      => ((A.Array a, A.Array a) %1-> Int -> Int -> Int -> (A.Array a, A.Array a))
      -> (A.Array a %1-> A.Array a)
      -> A.Array a %1-> A.Array a
sort' copyFn sortInplaceFn = Unsafe.toLinear go
  where
    go src =
      let (Ur n, src1) = A.size src
          (src2, dst)  = copyFn (src1, A.makeNoFill n) 0 0 n
      in src2 `lseq` sortInplaceFn dst

sortInplace' :: (Ord a, A.Elt a)
             => (A.Array a -> A.Array a -> A.Array a)
             -> A.Array a %1-> A.Array a
sortInplace' f = Unsafe.toLinear go
  where
    go src =
      let (Ur n, src1) = A.size src
      in f src1 (A.makeNoFill n)

--------------------------------------------------------------------------------

sortParM, sortParM4, sortInplaceParM, sortInplaceParM4 ::
  (Ord a, A.Elt a) => A.Array a %1-> Par.Par (A.Array a)
sortParM         = sortParM' P.copyParM sortInplaceParM
sortParM4        = sortParM' P.copyParM sortInplaceParM4
sortInplaceParM  = sortInplaceParM' writeSort1ParM
sortInplaceParM4 = sortInplaceParM' writeSortParM4

sortParM' :: (Ord a, A.Elt a)
          => ((A.Array a, A.Array a) %1-> Int -> Int -> Int -> Par.Par (A.Array a, A.Array a))
          -> (A.Array a %1-> Par.Par (A.Array a))
          -> A.Array a %1-> Par.Par (A.Array a)
sortParM' copyFn sortInplaceFn = Unsafe.toLinear go
  where
    go src =
      let (Ur n, src1) = A.size src
      in do (src2, dst) <- copyFn (src1, A.makeNoFill n) 0 0 n
            dst1 <- sortInplaceFn dst
            pure $ src2 `lseq` dst1

sortInplaceParM' :: (Ord a, A.Elt a)
                 => (A.Array a -> A.Array a -> Par.Par (A.Array a))
                 -> A.Array a %1-> Par.Par (A.Array a)
sortInplaceParM' f = Unsafe.toLinear go
  where
    go src =
      let (Ur n, src1) = A.size src
      in f src1 (A.makeNoFill n)

--------------------------------------------------------------------------------
-- Parallel
--------------------------------------------------------------------------------

writeSortPar4, writeSort1Par, writeSort2Par ::
  (Ord a, A.Elt a) => A.Array a -> A.Array a -> A.Array a
writeSortPar4 src tmp =
  let (Ur n, src1) = A.size src in
    if n < 2048 then (tmp `lseq` Quick.sortInplace' src1) else
    -- if n < 2048 then writeSort1 src1 tmp else
      let (src_l, src_r)   = A.splitMid src
          (src_ll, src_lr) = A.splitMid src_l
          (src_rl, src_rr) = A.splitMid src_r
          (tmp_l, tmp_r)   = A.splitMid tmp
          (tmp_ll, tmp_lr) = A.splitMid tmp_l
          (tmp_rl, tmp_rr) = A.splitMid tmp_r
          src_ll1 = writeSortPar4 src_ll tmp_ll
          src_lr1 = writeSortPar4 src_lr tmp_lr
          src_rl1 = writeSortPar4 src_rl tmp_rl
          src_rr1 = writeSortPar4 src_rr tmp_rr
          merged1 = src_ll1 `par` src_lr1 `pseq` writeMergePar src_ll1 src_lr1 tmp_l
          merged2 = src_rl1 `par` src_rr1 `pseq` writeMergePar src_rl1 src_rr1 tmp_r
      in merged1 `par` merged2 `pseq` writeMergePar merged1 merged2 src

writeSort1Par src tmp =
  let (Ur n, src1) = A.size src in
    if n < 2048 then (tmp `lseq` Quick.sortInplace' src1) else
    -- if n < 2048 then writeSort1 src1 tmp else
      let (src_l, src_r) = A.splitMid src
          (tmp_l, tmp_r) = A.splitMid tmp
          tmp_l1 = writeSort2Par src_l tmp_l
          tmp_r1 = writeSort2Par src_r tmp_r
      in tmp_l1 `par` tmp_r1 `pseq`
         writeMergePar tmp_l1 tmp_r1 (A.unsafeJoin src_l src_r)

writeSort2Par src tmp =
  let (Ur n, src1) = A.size src in
    if n < 2048 then Quick.sortInplace' (P.copyParAndGetDst (src1, tmp) 0 0 n) else
    -- if n < 2048 then writeSort2 src1 tmp else
      let (src_l, src_r) = A.splitMid src
          (tmp_l, tmp_r) = A.splitMid tmp
          src_l1 = writeSort1Par src_l tmp_l
          src_r1 = writeSort1Par src_r tmp_r
      in src_l1 `par` src_r1 `pseq`
         writeMergePar src_l1 src_r1 (A.unsafeJoin tmp_l tmp_r)

writeMergePar :: (Ord a, A.Elt a) => A.Array a -> A.Array a -> A.Array a -> A.Array a
writeMergePar left0 right0 dst0 =
  let (Ur !nd, dst1) = A.size dst0 in
    if nd < 2048 then writeMerge left0 right0 dst0 else
      let (Ur !nl, left1)  = A.size left0
          (Ur !nr, right1) = A.size right0 in
        if nl == 0 then A.copyAndGetDst (right1, dst1) 0 0 nr else
          let ----------------------------------------
              mid1 = nl `div` 2
              (Ur pivot, left2) = A.unsafeGet left1 mid1
              (mid2, right2) = binarySearch pivot nr right1
              ----------------------------------------
              (left2_l, _) = A.slice left2 0 mid1
              (left2_r, _) = A.slice left2 (mid1+1) (nl-(mid1+1))
              (right2_l, right2_r) = A.splitAt right2 mid2
              dst2 = A.unsafeSet dst1 (mid1+mid2) pivot
              (dst2_l, _) = A.slice dst2 0 (mid1+mid2)
              (dst2_r, _) = A.slice dst2 (mid1+mid2+1) (nd-(mid1+mid2+1))
              ----------------------------------------
              dst_l1 = writeMergePar left2_l right2_l dst2_l
              dst_r1 = writeMergePar left2_r right2_r dst2_r
          in dst_l1 `par` dst_r1 `pseq`
             A.unsafeJoin dst_l1 dst_r1

--------------------------------------------------------------------------------

writeSortParM4, writeSort1ParM, writeSort2ParM ::
  (Ord a, A.Elt a) => A.Array a -> A.Array a -> Par.Par (A.Array a)
writeSortParM4 src tmp =
  let (Ur n, src1) = A.size src in
    if n < 2048 then pure $ (tmp `lseq` Quick.sortInplace' src1) else
    -- if n < 2048 then pure $ writeSort1 src1 tmp else
      let (src_l, src_r)   = A.splitMid src
          (src_ll, src_lr) = A.splitMid src_l
          (src_rl, src_rr) = A.splitMid src_r
          (tmp_l, tmp_r)   = A.splitMid tmp
          (tmp_ll, tmp_lr) = A.splitMid tmp_l
          (tmp_rl, tmp_rr) = A.splitMid tmp_r
      in do src_ll1_f <- Par.spawn_ $ writeSortParM4 src_ll tmp_ll
            src_lr1_f <- Par.spawn_ $ writeSortParM4 src_lr tmp_lr
            src_rl1_f <- Par.spawn_ $ writeSortParM4 src_rl tmp_rl
            src_rr1_f <- Par.spawn_ $ writeSortParM4 src_rr tmp_rr
            src_ll1 <- Par.get src_ll1_f
            src_lr1 <- Par.get src_lr1_f
            src_rl1 <- Par.get src_rl1_f
            src_rr1 <- Par.get src_rr1_f
            merged1_f <- Par.spawn_ $ writeMergeParM src_ll1 src_lr1 tmp_l
            merged2_f <- Par.spawn_ $ writeMergeParM src_rl1 src_rr1 tmp_r
            merged1 <- Par.get merged1_f
            merged2 <- Par.get merged2_f
            writeMergeParM merged1 merged2 src

writeSort1ParM src tmp =
  let (Ur n, src1) = A.size src in
    if n < 2048 then pure $ (tmp `lseq` Quick.sortInplace' src1) else
    -- if n < 2048 then pure $ writeSort1 src1 tmp else
      let (src_l, src_r) = A.splitMid src
          (tmp_l, tmp_r) = A.splitMid tmp
      in do tmp_l1_f <- Par.spawn_ $ writeSort2ParM src_l tmp_l
            tmp_r1 <- writeSort2ParM src_r tmp_r
            tmp_l1 <- Par.get tmp_l1_f
            writeMergeParM tmp_l1 tmp_r1 (A.unsafeJoin src_l src_r)

writeSort2ParM src tmp =
  let (Ur n, src1) = A.size src in
    if n < 2048 then (P.copyParMAndGetDst (src1, tmp) 0 0 n) >>= \a -> pure (Quick.sortInplace' a) else
    -- if n < 2048 then pure $ writeSort2 src1 tmp else
      let (src_l, src_r) = A.splitMid src
          (tmp_l, tmp_r) = A.splitMid tmp
      in do src_l1_f <- Par.spawn_ $ writeSort1ParM src_l tmp_l
            src_r1 <- writeSort1ParM src_r tmp_r
            src_l1 <- Par.get src_l1_f
            writeMergeParM src_l1 src_r1 (A.unsafeJoin tmp_l tmp_r)

writeMergeParM :: (Ord a, A.Elt a) => A.Array a -> A.Array a -> A.Array a -> Par.Par (A.Array a)
writeMergeParM left0 right0 dst0 =
  let (Ur !nd, dst1) = A.size dst0 in
    if nd < 2048 then pure $ writeMerge left0 right0 dst0 else
      let (Ur !nl, left1)  = A.size left0
          (Ur !nr, right1) = A.size right0 in
        if nl == 0 then pure $ A.copyAndGetDst (right1, dst1) 0 0 nr else
          let ----------------------------------------
              mid1 = nl `div` 2
              (Ur pivot, left2) = A.unsafeGet left1 mid1
              (mid2, right2) = binarySearch pivot nr right1
              ----------------------------------------
              (left2_l, _) = A.slice left2 0 mid1
              (left2_r, _) = A.slice left2 (mid1+1) (nl-(mid1+1))
              (right2_l, right2_r) = A.splitAt right2 mid2
              dst2 = A.unsafeSet dst1 (mid1+mid2) pivot
              (dst2_l, _) = A.slice dst2 0 (mid1+mid2)
              (dst2_r, _) = A.slice dst2 (mid1+mid2+1) (nd-(mid1+mid2+1))
              ----------------------------------------
          in do dst_l1_f <- Par.spawn_ $ writeMergeParM left2_l right2_l dst2_l
                dst_r1 <- writeMergeParM left2_r right2_r dst2_r
                dst_l1 <- Par.get dst_l1_f
                pure $ A.unsafeJoin dst_l1 dst_r1

--------------------------------------------------------------------------------

binarySearch :: (Ord a, A.Elt a) => a -> Int -> A.Array a -> (Int, A.Array a)
binarySearch query = go 0
  where
    go lo hi arr =
      let n = hi-lo
      in if n == 0
         then (lo, arr)
         else let mid = lo + (n `div` 2)
                  (Ur pivot, arr1) = A.unsafeGet arr mid
              in case query `compare` pivot of
                   LT -> go lo mid arr1
                   GT -> go (mid+1) hi arr1
                   EQ -> (mid, arr)

--------------------------------------------------------------------------------
-- Sequential
--------------------------------------------------------------------------------

writeSort1, writeSort2 :: (Ord a, A.Elt a) => A.Array a -> A.Array a -> A.Array a
writeSort1 src tmp =
  let (Ur n, src1) = A.size src in
    if n < 20 then tmp `lseq` Insertion.sortInplace src1 else
      let (src_l, src_r) = A.splitMid src
          (tmp_l, tmp_r) = A.splitMid tmp
          tmp_l1 = writeSort2 src_l tmp_l
          tmp_r1 = writeSort2 src_r tmp_r
      in writeMerge tmp_l1 tmp_r1 (A.unsafeJoin src_l src_r)

writeSort2 src tmp =
  let (Ur n, src1) = A.size src in
    if n < 20 then Insertion.sortInplace (A.copyAndGetDst (src1, tmp) 0 0 n) else
      let (src_l, src_r) = A.splitMid src
          (tmp_l, tmp_r) = A.splitMid tmp
          src_l1 = writeSort1 src_l tmp_l
          src_r1 = writeSort1 src_r tmp_r
      in writeMerge src_l1 src_r1 (A.unsafeJoin tmp_l tmp_r)

writeMerge :: (Ord a, A.Elt a) => A.Array a -> A.Array a -> A.Array a -> A.Array a
writeMerge left0 right0 = go 0 0 0
  where
    (Ur !n1, left) = A.size left0
    (Ur !n2, right) = A.size right0

    go !i1 !i2 !j dst
      | i1 == n1 =
        A.copyAndGetDst (right, dst) i2 j (n2-i2)
      | i2 == n2 =
        A.copyAndGetDst (left, dst) i1 j (n1-i1)
      | otherwise =
        let (Ur x1, _) = A.unsafeGet left i1
            (Ur x2, _) = A.unsafeGet right i2
        in if x1 `compare` x2 == LT
           then go (i1+1) i2 (j+1) (A.unsafeSet dst j x1)
           else go i1 (i2+1) (j+1) (A.unsafeSet dst j x2)
