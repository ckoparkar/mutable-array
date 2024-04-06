{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes  #-}

module Data.Array.Mutable.Primitive
  ( module Data.Array.Mutable.Primitive
  , P.Prim
  ) where

import           Prelude hiding ( splitAt, sum )
import qualified GHC.Exts as GHC
import           Control.DeepSeq ( NFData(..) )
import           Data.Unrestricted.Linear ( Ur(..), unur, Consumable(..), lseq )
import qualified Unsafe.Linear as Unsafe
import qualified Prelude.Linear as Linear
import qualified Data.Primitive.Types as P

import           Data.Array.Mutable.Primitive.Unlifted hiding ( lseq )
import qualified Data.Array.Mutable.Primitive.Unlifted as Unlifted

--------------------------------------------------------------------------------
-- Mutable, lifted array API
--------------------------------------------------------------------------------

data Array a = Array { _lo  :: {-# UNPACK #-} !Int
                     , _hi  :: {-# UNPACK #-} !Int
                     , _arr ::                !(Array# a)
                     }

instance (Show a, P.Prim a) => Show (Array a) where
  show a@(Array lo hi _arr) =
    "Array { lower = " ++ show lo ++ ", upper = " ++ show hi ++ ", arr = " ++
      (show $ unur (toList a)) ++ "}"

instance NFData a => NFData (Array a) where
  rnf (Array lo hi _arr) = rnf lo `seq` rnf hi `seq` ()

instance Consumable (Array a) where
  consume (Array i j arr) = i `lseq` j `lseq` arr `Unlifted.lseq` ()

{-# INLINE alloc #-}
alloc :: P.Prim a => Int -> a -> (Array a %1-> Ur b) %1-> Ur b
alloc n a f = f (make n a)

{-# INLINE make #-}
make :: P.Prim a => Int -> a -> Array a
make n@(GHC.I# n#) x = Array 0 n (make# n# x)

{-# INLINE allocNoFill #-}
allocNoFill :: P.Prim a => Int -> (Array a %1-> Ur b) %1-> Ur b
allocNoFill n f = f (makeNoFill n)

{-# INLINE makeNoFill #-}
makeNoFill :: P.Prim a => Int -> Array a
makeNoFill n@(GHC.I# n#) = Array 0 n (makeNoFill# n#)

{-# INLINE size #-}
size :: Array a %1-> (Ur Int, Array a)
size = Unsafe.toLinear go
  where
    go a@(Array lo hi _) = (Ur (hi-lo), a)

{-# INLINE get #-}
{-# INLINE unsafeGet #-}
get, unsafeGet :: P.Prim a => Array a %1-> Int -> (Ur a, Array a)
get = Unsafe.toLinear go
  where
    go a i = (get' a i, a)
    get' (Array lo@(GHC.I# lo#) hi arr) i@(GHC.I# i#) =
      checkBounds "get" (lo+i) (lo,hi) `seq`
        Ur (get# arr (lo# GHC.+# i#))

unsafeGet = Unsafe.toLinear go
  where
    go a i = (unsafeGet' a i, a)
    unsafeGet' (Array (GHC.I# lo#) _hi arr) (GHC.I# i#) =
      Ur (get# arr (lo# GHC.+# i#))

{-# INLINE set #-}
{-# INLINE unsafeSet #-}
set, unsafeSet :: P.Prim a => Array a %1-> Int -> a -> Array a
set = Unsafe.toLinear go
  where
    go (Array lo@(GHC.I# lo#) hi arr) i@(GHC.I# i#) a =
      checkBounds "set" (lo+i) (lo,hi) `seq`
        Array (GHC.I# lo#) hi (set# arr (lo# GHC.+# i#) a)

unsafeSet = Unsafe.toLinear go
  where
    go (Array (GHC.I# lo#) hi arr) (GHC.I# i#) a =
      Array (GHC.I# lo#) hi (set# arr (lo# GHC.+# i#) a)

{-# INLINE copy #-}
copy :: P.Prim a => (Array a, Array a) %1-> Int -> Int -> Int -> (Array a, Array a)
copy = Unsafe.toLinear go
  where
    go (s@(Array (GHC.I# lo1) _hi1 src),
        d@(Array (GHC.I# lo2) _hi2 dst))
       (GHC.I# src_offset)
       (GHC.I# dst_offset)
       (GHC.I# n#) =
      case copy# src (lo1 GHC.+# src_offset) dst (lo2 GHC.+# dst_offset) n# of
        dst_arr' -> (s, d { _arr = dst_arr' })

{-# INLINE checkBounds #-}
checkBounds :: String -> Int -> (Int, Int) -> ()
checkBounds msg i (lo,hi) =
  if i < lo || i > hi
  then (error $ msg ++ ": index out of bounds: " ++ show i ++ "," ++ show lo ++ "," ++ show hi)
  else ()

--------------------------------------------------------------------------------
-- Splits and joins are unsafe because they allow an array to be split
-- into multiple slices and join assumes (but doesn't verify) that the
-- slices being joined are backed by the same array.
--------------------------------------------------------------------------------

{-# INLINE slice #-}
slice :: Array a %1-> Int -> Int -> (Array a, Array a)
slice = Unsafe.toLinear go
  where
    go a@(Array l _r arr) l' r' = (Array (l+l') (l+r') arr, a)

{-# INLINE splitAt #-}
splitAt :: P.Prim a => Array a %1-> Int -> (Array a, Array a, Array a)
splitAt = Unsafe.toLinear go
  where
    go xs m =
      let (Ur n, xs1) = size xs
          (s1, xs2) = slice xs1 0 m
          (s2, xs3) = slice xs2 m n
      in (s1, s2, xs3)

{-# INLINE splitMid #-}
splitMid :: P.Prim a => Array a %1-> (Array a, Array a, Array a)
splitMid = Unsafe.toLinear go
  where
    go xs =
      let (Ur n, xs1) = size xs
          m = n `div` 2
      in splitAt xs1 m

{-# INLINE join #-}
{-# INLINE unsafeJoin #-}
-- | PRE-CONDITION: the two slices are backed by the same array and should be contiguous.
join, unsafeJoin :: Array a %1-> Array a %1-> Array a
join = Unsafe.toLinear2 go
  where
    go a@(Array lo1 hi1 _arr1) b@(Array lo2 hi2 _arr2) =
      check (lo1,hi1) (lo2, hi2) `seq` unsafeJoin a b
    check (lo1,hi1) (lo2, hi2) =
      if lo1 <= hi1 &&
         hi1 == lo2 &&
         lo2 <= hi2
      then ()
      else error $ "join: slice indices not correct: " ++ show (lo1,hi1) ++
                   " and " ++ show (lo2, hi2)

unsafeJoin (Array lo1 _hi1 arr1) (Array _lo2 hi2 _arr2) =
  _hi1 `lseq` _lo2 `lseq` _arr2 `Unlifted.lseq` (Array lo1 hi2 arr1)

--------------------------------------------------------------------------------

{-# INLINE swap #-}
swap :: P.Prim a => Array a %1-> Int -> Int -> Array a
swap = Unsafe.toLinear go
  where
    go xs i j =
      let (Ur !xi, xs1) = get xs i
          (Ur xj, xs2) = get xs1 j
          xs3 = set xs2 i xj
          xs4 = set xs3 j xi
      in xs4

{-# INLINE fromList #-}
fromList :: P.Prim a => [a] -> (Array a %1-> Ur b) %1-> Ur b
fromList ls f =
  let arr = makeNoFill (length ls)
  in f (foldl (\acc (i,x) -> set acc i x) arr (zip [0..] ls))

{-# INLINE toList #-}
toList :: P.Prim a => Array a %1-> Ur [a]
toList = Unsafe.toLinear go
  where
    go (Array lo hi arr) =
      let ixs = [lo..(hi-1)]
      in Ur [ get# arr i# | (GHC.I# i#) <- ixs ]

--------------------------------------------------------------------------------

{-# INLINE sum #-}
sum :: (Num a, P.Prim a) => Array a %1-> Ur a
sum a0 = size a0 Linear.& \(Ur n, a1) -> (go a1 0 n 0)
  where
    go :: (Num a, P.Prim a) => Array a %1 -> Int -> Int -> a -> Ur a
    go xs0 i n acc
      | i == n    = xs0 `lseq` Ur acc
      | otherwise = unsafeGet xs0 i Linear.&
                    \(Ur xi, xs1) -> go xs1 (i+1) n (acc+xi)
