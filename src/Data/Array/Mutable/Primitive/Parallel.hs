{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Mutable.Primitive.Parallel where

import           GHC.Conc ( par, pseq )
import           Data.Unrestricted.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Control.Monad.Par as Par (Par, spawn_, get)

import qualified Data.Array.Mutable.Primitive as A

--------------------------------------------------------------------------------

sumPar :: (Num a, A.Prim a) => A.Array a %1-> Ur a
sumPar = Unsafe.toLinear go2
  where
    go2 a0 = Ur (go a0)
    go a0 =
      let (Ur n, a1) = A.size a0 in
        if n <= 2048 then unur (A.sum a1) else
          let (sl,sr,_a2) = A.splitMid a1
              suml = go sl
              sumr = go sr
          in suml `par` sumr `pseq` (suml + sumr)


sumParM :: (Num a, A.Prim a) => A.Array a %1-> Par.Par (Ur a)
sumParM = Unsafe.toLinear go2
  where
    go2 a0 = Ur <$> (go a0)
    go a0 =
      let (Ur n, a1) = A.size a0 in
        if n <= 2048 then pure (unur (A.sum a1))
        else let (sl,sr,_a2) = A.splitMid a1 in
               do suml_f <- Par.spawn_ $ go sl
                  !sumr <- go sr
                  !suml <- Par.get suml_f
                  pure $ (suml + sumr)
