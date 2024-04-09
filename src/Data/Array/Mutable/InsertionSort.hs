{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Mutable.InsertionSort ( sortInplace ) where

import           Data.Unrestricted.Linear ( Ur(..) )
import qualified Unsafe.Linear as Unsafe
import qualified Data.Array.Mutable.Primitive as A

--------------------------------------------------------------------------------

sortInplace :: forall a. (Ord a, A.Elt a) => A.Array a %1-> A.Array a
sortInplace = Unsafe.toLinear go
  where
    go src0 = let (Ur n, src1) = A.size src0
              in outerloop 1 n src1

    outerloop :: Int {- outer loop idx -}
              -> Int {- end -}
              -> A.Array a
              -> A.Array a
    outerloop i end src
      | i == end  = src
      | otherwise = let (Ur xi, src1) = A.unsafeGet src i
                    in innerloop i xi src1
      where
        innerloop :: Int {- inner loop idx -}
                  -> a   {- element at i -}
                  -> A.Array a
                  -> A.Array a
        innerloop j xi src0
          | j == 0 =
            outerloop (i+1) end src0
          | otherwise =
            let (Ur xjprev, src1) = A.unsafeGet src0 (j-1)
            in if xjprev `compare` xi == GT
               then let src2 = A.swap src1 j (j-1)
                    in innerloop (j-1) xi src2
               else let src2 = A.unsafeSet src1 j xi
                    in outerloop (i+1) end src2
