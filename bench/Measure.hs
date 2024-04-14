{-# LANGUAGE LinearTypes  #-}
{-# LANGUAGE BangPatterns #-}

module Measure ( run, bench ) where

import Control.Exception ( evaluate )
import Control.DeepSeq ( NFData(..), ($!!), force )
import Data.List ( sort )
import System.Mem ( performMajorGC )
import Data.Time.Clock ( getCurrentTime, diffUTCTime )

--------------------------------------------------------------------------------

median :: [Double] -> Double
median ls = (sort ls) !! (length ls `div` 2)

--------------------------------------------------------------------------------

run :: (Show b)
    => ((a %n-> b) -> a -> Int -> IO (b, Double, Double))
    -> String -> (a %n-> b) -> a -> Int -> Int -> IO ()
run bnch msg f x size iters = do
  putStrLn $ "BENCHMARK: " ++ msg
  (res, t_one, t_all) <- bnch f x iters
  let show' y = let str = show y
                in take 100 str ++ (if length str > 100 then "..." else "")
  putStrLn $ "SIZE: " ++ show size
  putStrLn $ "ITERS: " ++ show iters
  putStrLn $ "BATCHTIME: " ++ show t_all
  putStrLn $ "SELFTIMED: " ++ show t_one
  putStrLn $ "VAL: " ++ show' res
  putStrLn ""

bench :: (NFData a, Show b, NFData b) => (a %n-> b) -> a -> Int -> IO (b, Double, Double)
bench f arg iters = do
    let !arg2 = force arg
    !tups <- mapM (\_ -> dotrial f arg2) [1..iters]
    let (results, times) = unzip tups
    let selftimed = median times
        batchtime = sum times
    return $!! (last results, selftimed, batchtime)

{-# NOINLINE dotrial #-}
dotrial :: (NFData a, Show b, NFData b) => (a %n-> b) -> a -> IO (b, Double)
dotrial f arg = do
    performMajorGC
    t1 <- getCurrentTime
    !a <- evaluate $ (f arg)
    t2 <- getCurrentTime
    let delt = fromRational (toRational (diffUTCTime t2 t1))
    putStrLn ("iter time: " ++ show delt)
    return $!! (a,delt)
