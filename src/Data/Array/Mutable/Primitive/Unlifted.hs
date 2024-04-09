{-# LANGUAGE UnliftedNewtypes     #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE LinearTypes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Array.Mutable.Primitive.Unlifted where

import           Data.Proxy
import qualified GHC.Exts as GHC
import qualified Unsafe.Linear as Unsafe
import qualified Data.Primitive.Types as P

--------------------------------------------------------------------------------
-- Mutable, unlifted array API only for primitive types
--------------------------------------------------------------------------------

newtype Array# a = Array# (GHC.MutableByteArray# GHC.RealWorld)

{-# NOINLINE makeNoFill# #-}
makeNoFill# :: forall a. P.Prim a => GHC.Int# -> Array# a
makeNoFill# len =
  case GHC.runRW# (GHC.newByteArray# nbytes) of
    (# _, arr #) -> Array# arr
  where
    nbytes = (P.sizeOfType# (Proxy :: Proxy a)) GHC.*# len

{-# NOINLINE make# #-}
make# :: forall a. P.Prim a => GHC.Int# -> a -> Array# a
make# len elt =
  case GHC.runRW# (GHC.newByteArray# nbytes) of
    (# _, arr #) -> Array# (fill# arr len elt)
  where
    nbytes = (P.sizeOfType# (Proxy :: Proxy a)) GHC.*# len

{-# NOINLINE fill# #-}
fill# :: P.Prim a => GHC.MutableByteArray# GHC.RealWorld -> GHC.Int# -> a -> GHC.MutableByteArray# GHC.RealWorld
fill# arr len elt =
  case GHC.runRW# (P.setByteArray# arr 0# len elt) of
    _ -> arr

{-# NOINLINE get# #-}
get# :: P.Prim a => Array# a -> GHC.Int# -> a
get# (Array# arr) i =
  case GHC.runRW# (P.readByteArray# arr i) of
    (# _, ret #) -> ret

{-# NOINLINE set# #-}
set# :: P.Prim a => Array# a -> GHC.Int# -> a -> Array# a
set# (Array# arr) i a =
  case GHC.runRW# (P.writeByteArray# arr i a) of
    _ -> Array# arr

{-# NOINLINE copy# #-}
copy# :: forall a. P.Prim a => Array# a -> GHC.Int# -> Array# a -> GHC.Int# -> GHC.Int# -> Array# a
copy# (Array# src) src_offset (Array# dst) dst_offset n =
  case GHC.runRW# (GHC.copyMutableByteArray# src src_offset_bytes dst dst_offset_bytes n_bytes) of
    _ -> Array# dst
  where
    sz               = P.sizeOfType# (Proxy :: Proxy a)
    src_offset_bytes = sz GHC.*# src_offset
    dst_offset_bytes = sz GHC.*# dst_offset
    n_bytes          = sz GHC.*# n

{-# NOINLINE size# #-}
size# :: Array# a -> GHC.Int#
size# (Array# arr) =
  case GHC.runRW# (GHC.getSizeofMutableByteArray# arr) of
    (# _, sz #) -> sz

{-# NOINLINE dup2# #-}
dup2# :: P.Prim a => Array# a -> GHC.Int# -> (# Array# a, Array# a #)
dup2# old n =
  let new = makeNoFill# n
  in (# old, copy# old 0# new 0# n #)

--------------------------------------------------------------------------------

{-# INLINE toList# #-}
toList# :: P.Prim a => Array# a -> Int -> [a]
toList# arr n =
  let ixs = [0..n]
  in [ get# arr i | (GHC.I# i) <- ixs ]

{-# INLINE fromList# #-}
fromList# :: P.Prim a => [a] -> Array# a
fromList# [] = make# 0# undefined
fromList# ls =
  let !(GHC.I# len) = length ls
      a0 = make# len (head ls)
  in go a0 (zip [0..] ls)
  where
    go acc []                 = acc
    go acc ((GHC.I# i,x):rst) = go (set# acc i x) rst

{-# INLINE lseq #-}
lseq :: Array# a %1-> b %1-> b
lseq = Unsafe.toLinear2 (\_ b -> b)
