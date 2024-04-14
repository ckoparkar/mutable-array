{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Test.Tasty ( TestTree, testGroup, defaultMain )
import           Test.Tasty.QuickCheck
import qualified Prelude.Linear as Linear hiding ((>))
import qualified Unsafe.Linear as Unsafe
import           Data.Unrestricted.Linear
import qualified Data.List as L
import qualified Control.Monad.Par as Par

import qualified Data.Array.Mutable.Prelude as A
import qualified Data.Array.Mutable.Primitive as A
import qualified Data.Array.Mutable.Sort.Insertion as Insertion
import qualified Data.Array.Mutable.Sort.Merge as Merge
import qualified Data.Array.Mutable.Sort.Cilk as Cilk
import qualified Data.Array.Mutable.Parallel as P

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests, propTests :: TestTree
tests = testGroup "Tests" [ propTests ]

propTests =
  testGroup "Property tests"
    [ allocAndGet
    , makeSetAndGet
    , size
    , copy
    , splitAndCopy
    , swap
    , listRoundtrip
    , split
    , join
    , tsum
    , sumpar
    , sumparm
    , gen
    , genpar
    , genparm
    , fold
    , insertionsort
    , mergesort
    , cilksort
    ]
  where
    allocAndGet = testProperty "Alloc, get"  $
      \((NonNegative n) :: NonNegative Int) (x :: Int) ->
        unur (A.alloc n x A.toList)
        === replicate n x

    makeSetAndGet = testProperty "Make, set, get" $
      \((NonNegative n) :: NonNegative Int) (x :: Int) ->
        unur (A.allocNoFill n (\a0 -> setN a0 0 x n Linear.& A.toList))
        === take n [x,x+1..]

    size = testProperty "Size" $
      \((NonNegative n) :: NonNegative Int) ->
        unur (A.fromList [1..(n+1)] (\a0 -> A.size a0 Linear.&
                                     \(m,a1) -> a1 `lseq` m))
        === n+1

    copy = testProperty "Copy" $
      \((NonNegative n) :: NonNegative Int) ->
        let ls = [0..n] :: [Int] in
          unur (A.fromList ls
                (\src -> A.allocNoFill (length ls)
                  (\dst -> A.copy (src,dst) 0 0 (length ls)       Linear.&
                   \(src1,dst1) -> (A.toList src1, A.toList dst1) Linear.&
                   \(Ur src2, Ur dst2) -> Ur (src2, dst2))))
          === (ls, ls)

    splitAndCopy = testProperty "Split and copy" $
      \((NonNegative n0) :: NonNegative Int) ->
        let ls = [0..n0+1] :: [Int]
            n = length ls
            m = n `div` 2 in
          unur (A.fromList ls
                (\src -> A.allocNoFill (length ls)
                  (\dst -> A.splitAt src m Linear.&
                   \(sl,sr)    ->  A.copy (sl,dst) 0 0 m                Linear.&
                   \(sl1,dst1) -> sl1 `lseq` A.copy (sr,dst1) 0 m (n-m) Linear.&
                   \(sr1,dst2) -> sr1 `lseq` A.toList dst2)))
          === ls

    swap = testProperty "Swap" $
      \((NonNegative n) :: NonNegative Int) (x :: Int) ->
        let m = n+2 in
          unur (A.allocNoFill m (\a0 -> setN a0 0 x m     Linear.&
                                 \a1 -> A.swap a1 0 (m-1) Linear.&
                                 A.toList))
          === swapFirstAndLast (take m [x,x+1..])

    listRoundtrip = testProperty "List roundtrip" $
      \((NonEmpty ls) :: NonEmptyList Int) ->
        unur (A.fromList ls A.toList) === ls

    split = testProperty "Split" $
      \((NonNegative n) :: NonNegative Int) ->
        let ls = [0..n] :: [Int] in
          unur (A.fromList ls (\a0 -> A.splitMid a0                   Linear.&
                               \(sl,sr) -> (A.toList sl, A.toList sr) Linear.&
                               \(Ur a, Ur b) -> Ur (a,b)))
          === splitAt ((n+1) `div` 2) ls

    join = testProperty "Join" $
      \((NonNegative n) :: NonNegative Int) ->
        let ls = [0..n] :: [Int] in
          unur (A.fromList ls (\a0 -> A.splitMid a0     Linear.&
                               \(sl,sr) -> A.join sl sr Linear.&
                               A.toList ))
          === ls

    tsum = testProperty "Sum" $
      \((NonEmpty ls) :: NonEmptyList Int) ->
        unur (A.fromList ls A.sum)
        === sum ls

    sumpar = testProperty "Parallel sum" $
      \((NonEmpty ls) :: NonEmptyList Int) ->
        let ls1 = take 3000 (cycle ls) in
          unur (A.fromList ls1 P.sumPar)
          === unur (A.fromList ls1 A.sum)

    sumparm = testProperty "Parallel sum (ParM)" $
      \((NonEmpty ls) :: NonEmptyList Int) ->
        let ls1 = take 3000 (cycle ls) in
          unur (A.fromList ls1 ((Unsafe.toLinear Par.runPar) Linear.. P.sumParM))
          === unur (A.fromList ls1 A.sum)

    gen = testProperty "Generate" $
      \((NonNegative n) :: NonNegative Int) ->
        unur (A.generate n (*2) A.toList)
        === map (*2) [0..n-1]

    genpar = testProperty "Parallel generate" $
      \((NonNegative n) :: NonNegative Int) ->
        let m = 3000+n in
          unur (P.generatePar m (*2) A.toList)
          === unur (A.generate m (*2) A.toList)

    genparm = testProperty "Parallel generate (ParM)" $
      \((NonNegative n) :: NonNegative Int) ->
        let m = 3000+n in
          unur (Par.runPar (P.generateParM m (*2) ((Unsafe.toLinear pure) Linear.. A.toList)))
          === unur (A.generate m (*2) A.toList)

    fold = testProperty "Fold" $
      \((NonEmpty ls) :: NonEmptyList Int) ->
        unur (A.fromList ls (A.foldl (Unsafe.toLinear2 (+)) 0))
        === sum ls

    insertionsort = testProperty "Insertion sort" $
      \((NonEmpty ls) :: NonEmptyList Int) ->
        unur (A.fromList ls (A.toList Linear.. Insertion.sortInplace))
        === L.sort ls

    mergesort = testProperty "Merge sort" $
      \((NonEmpty ls) :: NonEmptyList Int) ->
        unur (A.fromList ls (A.toList Linear.. Merge.sortInplace))
        === L.sort ls

    cilksort = testProperty "Cilk sort" $
      \((NonEmpty ls) :: NonEmptyList Int) ->
        let ls1 = take 3000 (cycle ls) in
          unur (A.fromList ls1 (A.toList Linear.. Cilk.sortInplacePar))
          === L.sort ls1

setN :: A.Array Int %1-> Int -> Int -> Int -> A.Array Int
setN arr i x n
  | i == n    = arr
  | otherwise = setN (A.set arr i x) (i+1) (x+1) n

swapFirstAndLast :: [a] -> [a]
swapFirstAndLast ls
  | []  <- ls = []
  | [x] <- ls = [x]
  | otherwise = last ls : (init . tail $ ls) ++ [head ls]
