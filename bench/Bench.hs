{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Criterion          ( Benchmark, bench, bgroup, nf )
import           Criterion.Main     ( defaultMain )
import           Control.DeepSeq    ( NFData, force)
import           Data.Proxy         ( Proxy(..) )
import           Data.Int           ( Int64 )
import           Data.List.Split    ( splitOn )
import           System.Random      ( Random, newStdGen, randoms )
import           System.Environment ( getArgs, withArgs )
import           Data.Unrestricted.Linear ( Ur(..), unur )
import qualified Unsafe.Linear as Unsafe
import qualified Prelude.Linear as Linear
import           Control.Monad ( forM_ )
import qualified Control.Monad.Par as Par ( runPar )

import qualified Measure as M
import qualified Data.Array.Mutable.Parallel as P
import qualified Data.Array.Mutable.Primitive as A
import qualified Data.Array.Mutable.Prelude as A
import qualified Data.Array.Mutable.Sort.Cilk as Cilk
import qualified Data.Array.Mutable.Sort.Quick as Quick

--------------------------------------------------------------------------------

data Benchmarks = SumArray | GenArray | CopyArray
                | CilkSort | QuickSort
  deriving (Eq, Show, Read)

main :: IO ()
main = do
  allargs <- getArgs
  let usage = "USAGE: benchrunner -- BENCH_ARGS -- CRITERION_ARGS"
      isHelp = ("--help" ==)
      (benchmark,size,rst) =
        case splitOn ["--"] allargs of
          [] -> (SumArray,10,[])
          ((bnch:sz:_):rst') ->
            if isHelp bnch || isHelp sz then error usage
            else (read bnch :: Benchmarks, read sz :: Int, if null rst' then [] else (head rst'))
          _ -> error usage
  runbench <-
    case benchmark of
      SumArray  -> bSumArray (Proxy :: Proxy Int64) size
      GenArray  -> bGenArray (Proxy :: Proxy Int64) size
      CopyArray -> bCopyArray (Proxy :: Proxy Int64) size
      CilkSort  -> bCilkSort (Proxy :: Proxy Int64) size
      QuickSort -> bQuickSort (Proxy :: Proxy Int64) size
  withArgs rst $ defaultMain [ runbench ]

--------------------------------------------------------------------------------

iters :: Int
iters = 5

bSumArray :: forall a. (Show a, Random a, NFData a, Num a, A.Elt a) =>
             Proxy a -> Int -> IO Benchmark
bSumArray _ty size = do
  rng <- newStdGen
  let ls = take size (randoms rng :: [a])
      !input = force (unur (A.fromList ls (Unsafe.toLinear Ur)))
  b False "SumArray" size input fseq fpar fparm
  where
    fseq = (unur Linear.. A.sum)
    fpar = (unur Linear.. P.sumPar)
    fparm = (unur Linear.. (Unsafe.toLinear Par.runPar) Linear.. P.sumParM)

bGenArray :: forall a. (Show a, Random a, NFData a, Num a, A.Elt a) =>
             Proxy a -> Int -> IO Benchmark
bGenArray _ty size = do
  let !input = force size
  b False "GenArray" size input fseq fpar fparm
  where
    fseq = (\n -> unur (A.generate n (*2) (Unsafe.toLinear Ur)))
    fpar = (\n -> unur (P.generatePar n (*2) (Unsafe.toLinear Ur)))
    fparm = (\n -> unur (Par.runPar (P.generateParM n (*2) (Unsafe.toLinear (pure . Ur)))))

bCopyArray :: forall a. (Eq a, Show a, Random a, NFData a, Num a, A.Elt a) =>
              Proxy a -> Int -> IO Benchmark
bCopyArray _ty size = do
  rng <- newStdGen
  let ls = take size (randoms rng :: [a])
      !chk = force (sum ls)
      !input = force (unur (A.fromList ls (Unsafe.toLinear Ur)))
  b False "CopyArray" size (input, chk) fseq fpar fparm
  where
    fseq (arr, chk) =
      chk == unur (A.sum (snd (A.copy (arr, (A.makeNoFill size)) 0 0 size)))

    fpar (arr, chk) =
      chk == unur (P.sumPar (snd (P.copyPar (arr, (A.makeNoFill size)) 0 0 size)))

    fparm (arr, chk) =
      chk == unur (Par.runPar ((\(_s,d) -> P.sumParM d) =<<
                               (P.copyParM (arr, (A.makeNoFill size)) 0 0 size)))

bCilkSort :: forall a. (Ord a, Show a, Random a, NFData a, Num a, A.Elt a, Enum a) =>
             Proxy a -> Int -> IO Benchmark
bCilkSort _ty size = do
  -- rng <- newStdGen
  -- let ls = take size (randoms rng :: [a])
  --     !input = force (unur (A.fromList ls (Unsafe.toLinear Ur)))
  let ls = [0..(fromIntegral size - 1)] :: [a]
      !input = force (unur (A.fromList ls ((Unsafe.toLinear Ur) Linear.. (scramble size))))
  b False "CilkSort" size input fseq fpar fparm
  where
    fseq, fpar, fparm :: A.Array a %1-> A.Array a
    fseq = Cilk.sort
    fpar = Cilk.sortPar
    fparm = (Unsafe.toLinear Par.runPar) Linear.. Cilk.sortParM

bQuickSort :: forall a. (Ord a, Show a, Random a, NFData a, Num a, A.Elt a) =>
             Proxy a -> Int -> IO Benchmark
bQuickSort _ty size = do
  -- rng <- newStdGen
  -- let ls = take size (randoms rng :: [a])
  let ls = [0..size-1]
      !input = force (unur (A.fromList ls ((Unsafe.toLinear Ur) Linear.. (scramble size))))
  b True "QuickSort" size input fseq fseq fseq
  where
    fseq = Quick.sort

scramble :: A.Elt a => Int -> A.Array a %1-> A.Array a
scramble n = Unsafe.toLinear (go 1 0)
  where
    myrand curr = curr * 1103515245 + 12345
    go rng i arr
      | i == n    = arr
      | otherwise = let rng1 = myrand rng
                        j = rng1 `mod` n
                        arr1 = A.swap arr i j
                    in go rng1 (i+1) arr1

b :: forall a b n. (NFData a, Show b, NFData b)
  => Bool -> String -> Int
  -> a -> (a %n-> b) -> (a %n-> b) -> (a %n-> b)
  -> IO Benchmark
b seqOnly msg size input fseq fpar fparm = do
  let ls1 = (msg ++ "/Seq", fseq) :
            (if seqOnly
             then []
             else [ (msg ++ "/Par", fpar)
                  , (msg ++ "/ParM", fparm)
                  ])

      ls2 = (bench "Seq" (nf (Unsafe.toLinear fseq) input)) :
            (if seqOnly
             then []
             else [ bench "Par" (nf (Unsafe.toLinear fpar) input)
                  , bench "ParM" (nf (Unsafe.toLinear fparm) input)
                  ])
  forM_ ls1 $ \(msg1,f) -> M.run M.bench msg1 f input size iters
  pure $ bgroup msg ls2
