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

--------------------------------------------------------------------------------

data Benchmarks = SumArray | GenArray
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
      SumArray -> bSumArray (Proxy :: Proxy Int64) size
      GenArray -> bGenArray (Proxy :: Proxy Int64) size
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
  b "SumArray" size input fseq fpar fparm
  where
    fseq = (unur Linear.. A.sum)
    fpar = (unur Linear.. P.sumPar)
    fparm = (unur Linear.. (Unsafe.toLinear Par.runPar) Linear.. P.sumParM)

bGenArray :: forall a. (Show a, Random a, NFData a, Num a, A.Elt a) =>
             Proxy a -> Int -> IO Benchmark
bGenArray _ty size = do
  let !input = force size
  b "GenArray" size input fseq fpar fparm
  where
    fseq = (\n -> unur (A.generate n (*2) (Unsafe.toLinear Ur)))
    fpar = (\n -> unur (P.generatePar n (*2) (Unsafe.toLinear Ur)))
    fparm = (\n -> unur (Par.runPar (P.generateParM n (*2) (Unsafe.toLinear (pure . Ur)))))

b :: forall a b. (NFData a, Show b, NFData b)
  => String -> Int
  -> a -> (a -> b) -> (a -> b) -> (a -> b)
  -> IO Benchmark
b msg size input fseq fpar fparm = do
  forM_ [ (msg ++ "/Seq", fseq)
        , (msg ++ "/Par", fpar)
        , (msg ++ "/ParM", fparm)
        ] $
    \(msg1,f) -> M.run M.bench msg1 f input size iters
  let critbench = bgroup msg
        [ bench "Seq" (nf fseq input)
        , bench "Par" (nf fpar input)
        , bench "ParM" (nf fparm input)
        ]
  pure critbench
