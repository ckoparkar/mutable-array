{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Mutable.Parallel where

import           GHC.Conc ( par, pseq )
import           Data.Unrestricted.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Prelude.Linear as Linear
import qualified Control.Monad.Par as Par (Par, spawn_, get)

import qualified Data.Array.Mutable.Primitive as A
import qualified Data.Array.Mutable.Prelude as A

--------------------------------------------------------------------------------

sumPar :: (Num a, A.Elt a) => A.Array a %1-> Ur a
sumPar = Unsafe.toLinear go2
  where
    go2 a0 = Ur (go a0)
    go a0 =
      let (Ur !n, a1) = A.size a0 in
        if n <= 2048 then unur (A.sum a1) else
          let (sl,sr) = A.splitMid a1
              suml = go sl
              sumr = go sr
          in suml `par` sumr `pseq` (suml + sumr)


sumParM :: (Num a, A.Elt a) => A.Array a %1-> Par.Par (Ur a)
sumParM = Unsafe.toLinear go2
  where
    go2 a0 = Ur <$> (go a0)
    go a0 =
      let (Ur !n, a1) = A.size a0 in
        if n <= 2048 then pure (unur (A.sum a1))
        else let (sl,sr) = A.splitMid a1 in
               do suml_f <- Par.spawn_ $ go sl
                  !sumr <- go sr
                  !suml <- Par.get suml_f
                  pure $ (suml + sumr)

generatePar :: (Num a, A.Elt a) => Int -> (Int -> a) -> (A.Array a %1-> Ur b) %1-> Ur b
generatePar n g f = f (go 0 (A.makeNoFill (n `max` 0)))
  where
    go off a0 =
      let (Ur !m, a1) = A.size a0 in
        if m <= 2048 then (A.generate' m off g a1) else
          let h = m `div` 2
              (sl,sr) = A.splitAt a1 h
              genl = go off sl
              genr = go (off+h) sr
          in genl `par` genr `pseq` (A.join genl genr)

generateParM :: forall a b. (Num a, A.Elt a) => Int -> (Int -> a)
             -> (A.Array a %1-> Par.Par (Ur b)) %1-> Par.Par (Ur b)
generateParM n g = Unsafe.toLinear go2
  where
    go2 :: (A.Array a %1-> Par.Par (Ur b)) -> Par.Par (Ur b)
    go2 f = (\x -> f x) =<< (go 0 (A.makeNoFill (n `max` 0)))
    go off a0 =
      let (Ur !m, a1) = A.size a0 in
        if m <= 2048 then pure (A.generate' m off g a1) else
          let h = m `div` 2
              (sl,sr) = A.splitAt a1 h in
            do genl_f <- Par.spawn_ $ go off sl
               genr <- go (off+h) sr
               genl <- Par.get genl_f
               pure $ A.join genl genr

copyPar :: A.Elt a => (A.Array a, A.Array a) %1-> Int -> Int -> Int -> (A.Array a, A.Array a)
copyPar = Unsafe.toLinear go2
  where
    go2 (src,dst) soff doff n =
      let (cpysrc, src1) = A.slice src soff n
          (cpydst, dst1) = A.slice dst doff n
          (cpysrc2, cpydst2) = go cpysrc cpydst n
      in cpysrc2 `seq` cpydst2 `seq` (src1, dst1)

    go src dst !n
      | n <= 2048 =
        A.copy (src,dst) 0 0 n
      | otherwise =
        let h = n `div` 2
            (sl,sr) = A.splitAt src h
            (dl,dr) = A.splitAt dst h
            cl = go sl dl h
            cr = go sr dr (n-h)
        in cl `par` cr `pseq` (A.join (fst cl) (fst cr), A.join (snd cl) (snd cr))

copyParM :: A.Elt a => (A.Array a, A.Array a) %1-> Int -> Int -> Int -> Par.Par (A.Array a, A.Array a)
copyParM = Unsafe.toLinear go2
  where
    go2 (src,dst) soff doff n = do
      let (cpysrc, src1) = A.slice src soff n
          (cpydst, dst1) = A.slice dst doff n
      (cpysrc2, cpydst2) <- go cpysrc cpydst n
      pure $ cpysrc2 `lseq` cpydst2 `lseq` (src1, dst1)

    go src dst !n
      | n <= 2048 =
        pure $ A.copy (src,dst) 0 0 n
      | otherwise =
        do let h = n `div` 2
               (sl,sr) = A.splitAt src h
               (dl,dr) = A.splitAt dst h
           cl_f <- Par.spawn_ $ go sl dl h
           cr <- go sr dr (n-h)
           cl <- Par.get cl_f
           pure $ (A.join (fst cl) (fst cr), A.join (snd cl) (snd cr))

{-# INLINE copyParAndGetDst #-}
copyParAndGetDst :: A.Elt a => (A.Array a, A.Array a) %1-> Int -> Int -> Int -> A.Array a
copyParAndGetDst (src,dst) i j n =
  copyPar (src,dst) i j n Linear.&
  \(src1,dst1) -> src1 `lseq` dst1

{-# INLINE copyParMAndGetDst #-}
copyParMAndGetDst :: A.Elt a => (A.Array a, A.Array a) %1-> Int -> Int -> Int -> Par.Par (A.Array a)
copyParMAndGetDst = Unsafe.toLinear go
  where
    go (src,dst) i j n =
      copyParM (src,dst) i j n >>=
      \(src1,dst1) -> pure (src1 `lseq` dst1)
