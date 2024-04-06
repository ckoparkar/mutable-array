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
import            Control.Monad ( forM_ )
import qualified Control.Monad.Par as Par ( runPar )

import qualified Measure as M
import qualified Data.Array.Mutable.Primitive.Parallel as P
import qualified Data.Array.Mutable.Primitive as A

--------------------------------------------------------------------------------

bSumArray :: forall a. (Show a, Random a, NFData a, Num a, A.Prim a) =>
             Proxy a -> Int -> IO Benchmark
bSumArray _ty size = do
  rng <- newStdGen
  let ls = take size (randoms rng :: [a])
      !input = force (unur (A.fromList ls (Unsafe.toLinear Ur)))
  forM_ [ ("Seq sum array", (unur Linear.. A.sum))
        , ("Par sum array", (unur Linear.. P.sumPar))
        , ("Par sum array", (unur Linear.. (Unsafe.toLinear Par.runPar) Linear.. P.sumParM))
        ] $
    \(msg,f) -> M.run M.bench msg f input size 9
  let critbench = bgroup "Criterion benchmarks"
        [ bench "Seq sum array" (nf (\a -> unur (A.sum a)) input)
        , bench "Par sum array" (nf (\a -> unur (P.sumPar a)) input)
        , bench "Par sum array" (nf (\a -> unur ((Unsafe.toLinear Par.runPar) (P.sumParM a))) input)
        ]
  pure critbench


data Benchmarks = SumArray
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
  withArgs rst $ defaultMain [ runbench ]
