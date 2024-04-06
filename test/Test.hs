{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Test.Tasty ( TestTree, testGroup, defaultMain )
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import qualified Prelude.Linear as Linear hiding ((>))
import           Data.Unrestricted.Linear

import qualified Data.Array.Mutable.Primitive as A
import qualified Data.Array.Mutable.Primitive.Parallel as P

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests, unitTests, propTests :: TestTree
tests = testGroup "Tests" [ unitTests, propTests ]

unitTests =
  testGroup "Unit tests"
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
    , parsum
    , gen
    , pargen
    ]
  where
    ls = [1,2,3,4] :: [Int]

    allocAndGet = testCase "Alloc, get"  $
      unur (A.alloc 4 (5 :: Int) A.toList)
        @?= [5,5,5,5]

    makeSetAndGet = testCase "Make, set, get" $
      unur (A.allocNoFill 4 (\a0 -> setN a0 0 10 4 Linear.& A.toList))
        @?= [10,11,12,13]

    size = testCase "Size" $
      unur (A.fromList ls (\a0 -> A.size a0 Linear.&
                           \(n,a1) -> a1 `lseq` n))
      @?= 4

    copy = testCase "Copy" $
      unur (A.fromList ls
             (\src -> A.allocNoFill (length ls)
               (\dst -> A.copy (src,dst) 0 0 (length ls)       Linear.&
                \(src1,dst1) -> (A.toList src1, A.toList dst1) Linear.&
                \(Ur src2, Ur dst2) -> Ur (src2, dst2))))
      @?= (ls, ls)

    splitAndCopy = testCase "Split and copy" $
      let n = length ls
          m = n `div` 2 in
        unur (A.fromList ls
               (\src -> A.allocNoFill (length ls)
                 (\dst -> A.splitAt src m Linear.&
                  \(sl,sr,src1) -> src1 `lseq` A.copy (sl,dst) 0 0 m     Linear.&
                  \(sl1,dst1)   -> sl1 `lseq` A.copy (sr,dst1) 0 m (n-m) Linear.&
                  \(sr1,dst2)   -> sr1 `lseq` A.toList dst2)))
        @?= ls

    swap = testCase "Swap" $
      unur (A.allocNoFill 4 (\a0 -> setN a0 0 10 4 Linear.&
                             \a1 -> A.swap a1 0 3  Linear.&
                             A.toList))
        @?= [13,11,12,10]

    listRoundtrip = testCase "List roundtrip" $
      unur (A.fromList ls A.toList)
      @?= ls

    split = testCase "Split" $
      unur (A.fromList ls (\a0 -> A.splitMid a0                                Linear.&
                           \(sl,sr,a1) -> a1 `lseq` (A.toList sl, A.toList sr) Linear.&
                           \(Ur a, Ur b) -> Ur (a,b)))
      @?= ([1,2], [3,4])

    join = testCase "Join" $
      unur (A.fromList ls (\a0 -> A.splitMid a0                  Linear.&
                           \(sl,sr,a1) -> a1 `lseq` A.join sl sr Linear.&
                           A.toList))
      @?= ls

    tsum = testCase "Sum" $
      unur (A.fromList ls A.sum)
      @?= sum ls

    parsum = testCase "Parallel sum" $
      unur (A.fromList (take 3000 ([1..] :: [Int])) A.sum)
      @?= unur (A.fromList (take 3000 ([1..] :: [Int])) P.sumPar)

    gen = testCase "Generate" $
      unur (A.generate 4 (*2) A.toList)
      @?= [0,2,4,6]

    pargen = testCase "Parallel generate" $
      let m = 3000
      in unur (P.generatePar m (*2) A.toList)
         @?= unur (A.generate m (*2) A.toList)


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
    , parsum
    , gen
    , pargen
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
                   \(sl,sr,src1) -> src1 `lseq` A.copy (sl,dst) 0 0 m     Linear.&
                   \(sl1,dst1)   -> sl1 `lseq` A.copy (sr,dst1) 0 m (n-m) Linear.&
                   \(sr1,dst2)   -> sr1 `lseq` A.toList dst2)))
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
          unur (A.fromList ls (\a0 -> A.splitMid a0                                Linear.&
                               \(sl,sr,a1) -> a1 `lseq` (A.toList sl, A.toList sr) Linear.&
                               \(Ur a, Ur b) -> Ur (a,b)))
          === splitAt ((n+1) `div` 2) ls

    join = testProperty "Join" $
      \((NonNegative n) :: NonNegative Int) ->
        let ls = [0..n] :: [Int] in
          unur (A.fromList ls (\a0 -> A.splitMid a0                  Linear.&
                               \(sl,sr,a1) -> a1 `lseq` A.join sl sr Linear.&
                               A.toList ))
          === ls

    tsum = testProperty "Sum" $
      \((NonEmpty ls) :: NonEmptyList Int) ->
        unur (A.fromList ls A.sum)
        === sum ls

    parsum = testProperty "Parallel sum" $
      \((NonEmpty ls) :: NonEmptyList Int) ->
        unur (A.fromList ls A.sum)
        === unur (A.fromList ls P.sumPar)

    gen = testProperty "Generate" $
      \((NonNegative n) :: NonNegative Int) ->
        unur (A.generate n (*2) A.toList)
        === map (*2) [0..n-1]

    pargen = testProperty "Parallel generate" $
      \((NonNegative n) :: NonNegative Int) ->
        let m = 3000+n in
          unur (P.generatePar m (*2) A.toList)
          === unur (A.generate m (*2) A.toList)

setN :: A.Array Int %1-> Int -> Int -> Int -> A.Array Int
setN arr i x n
  | i == n    = arr
  | otherwise = setN (A.set arr i x) (i+1) (x+1) n

swapFirstAndLast :: [a] -> [a]
swapFirstAndLast ls
  | []  <- ls = []
  | [x] <- ls = [x]
  | otherwise = last ls : (init . tail $ ls) ++ [head ls]
