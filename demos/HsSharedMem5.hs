{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{- |
   Shared Memory Demo 5: GHC Spark Parallelism.

   Demos 2–4 use C/OpenMP for the parallel half. This demo shows
   pure Haskell parallelism via GHC sparks, using parCombine to
   evaluate split partitions in parallel.

   The recursive pattern:
     1. halve the array (zero-copy)
     2. spark the left half's computation
     3. run the right half on the current thread
     4. parCombine waits for both and recombines

   Compare:
     - Sequential: all partitions on one Haskell thread
     - Parallel (sparks): partitions distributed via GHC's work-stealing

   Run: ./build/shared5_demo +RTS -N4
-}
module Main where

import Data.Array.Linear
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf
import Control.Monad (forM_)

------------------------------------------------------------------------
-- Haskell-side transform
------------------------------------------------------------------------

-- | f(x) = sin(x)*cos(x) + sqrt(abs(x))
transformElem :: Double -> Double
transformElem x = sin x * cos x + sqrt (abs x)

-- | Sequential transform: out[i] = f(in[base + i]).
seqTransform :: forall s si. RW s %1 -> DArray si -> DArray s -> Int -> RW s
seqTransform rw0 arrIn arrOut base = go rw0 0
  where
    n = size arrOut
    go :: RW s %1 -> Int -> RW s
    go rw i
        | i >= n = rw
        | otherwise =
            let (v, _) = unsafeRead MkRW arrIn (base + i)
            in go (unsafeWrite rw arrOut i (transformElem v)) (i + 1)

------------------------------------------------------------------------
-- Sequential multi-partition (combine)
------------------------------------------------------------------------

-- | Recursively split into 2^depth partitions, transform each sequentially.
-- Uses sequential combine — all work on one thread.
seqPartition :: forall s si. RW s %1 -> DArray si -> DArray s -> Int -> Int -> RW s
seqPartition rw arrIn arrOut base depth
    | depth <= 0 = seqTransform rw arrIn arrOut base
    | otherwise =
        case halve rw arrOut of
            MkSlice st rwL rwR arrL arrR ->
                let rwL' = seqPartition rwL arrIn arrL base (depth - 1)
                    rwR' = seqPartition rwR arrIn arrR (base + size arrL) (depth - 1)
                in combine st rwL' rwR'

------------------------------------------------------------------------
-- Parallel multi-partition (parCombine / sparks)
------------------------------------------------------------------------

-- | Recursively split into 2^depth partitions, transform each.
-- Uses parCombine to spark the left half for parallel evaluation.
-- GHC's work-stealing scheduler distributes sparks across capabilities.
parPartition :: forall s si. RW s %1 -> DArray si -> DArray s -> Int -> Int -> RW s
parPartition rw arrIn arrOut base depth
    | depth <= 0 = seqTransform rw arrIn arrOut base
    | otherwise =
        case halve rw arrOut of
            MkSlice st rwL rwR arrL arrR ->
                let rwL' = parPartition rwL arrIn arrL base (depth - 1)
                    rwR' = parPartition rwR arrIn arrR (base + size arrL) (depth - 1)
                in parCombine st rwL' rwR'

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

nowMs :: IO Double
nowMs = do
    ns <- getMonotonicTimeNSec
    return (fromIntegral ns / 1e6)

fillArray :: forall s. RW s %1 -> DArray s -> Int -> RW s
fillArray rw0 arr nn = go rw0 0
  where
    go :: RW s %1 -> Int -> RW s
    go rw i
        | i >= nn = rw
        | otherwise = go (unsafeWrite rw arr i (fromIntegral i * 0.001)) (i + 1)

------------------------------------------------------------------------
-- Correctness
------------------------------------------------------------------------

verifyCorrectness :: Int -> Int -> IO ()
verifyCorrectness n depth = do
    printf "--- Correctness check (N=%d, depth=%d) ---\n" n depth

    -- Reference: sequential, no partitioning
    let Ur refResult = alloc n \arrOut rwOut ->
            let (arrIn, rwIn0) = unsafeAlloc n
                !_rwIn = fillArray rwIn0 arrIn n
                rwOut' = seqTransform rwOut arrIn arrOut 0
            in freeze rwOut' arrOut

    -- Parallel: parCombine with sparks
    let Ur parResult = alloc n \arrOut rwOut ->
            let (arrIn, rwIn0) = unsafeAlloc n
                !_rwIn = fillArray rwIn0 arrIn n
                rwOut' = parPartition rwOut arrIn arrOut 0 depth
            in freeze rwOut' arrOut

    let maxDiff = maximum [ abs (a - b) | (a, b) <- zip refResult parResult ]
    printf "  Max diff: %.2e %s\n" maxDiff
        (if maxDiff < 1e-12 then "OK" else "MISMATCH" :: String)

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

benchSeq :: Int -> Int -> IO Double
benchSeq n depth = do
    let (arrIn,  rwIn0)  = unsafeAlloc n
        (arrOut, rwOut0) = unsafeAlloc n
        !_rwIn = fillArray rwIn0 arrIn n
    t0 <- nowMs
    let !_rwOut = seqPartition rwOut0 arrIn arrOut 0 depth
    t1 <- nowMs
    return (t1 - t0)

benchPar :: Int -> Int -> IO Double
benchPar n depth = do
    let (arrIn,  rwIn0)  = unsafeAlloc n
        (arrOut, rwOut0) = unsafeAlloc n
        !_rwIn = fillArray rwIn0 arrIn n
    t0 <- nowMs
    let !_rwOut = parPartition rwOut0 arrIn arrOut 0 depth
    t1 <- nowMs
    return (t1 - t0)

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Shared Memory Demo 5: GHC Spark Parallelism ==="
    putStrLn "    (linear types + parCombine for pure Haskell parallelism)"
    putStrLn ""

    -- Correctness
    verifyCorrectness 10000 3
    putStrLn ""

    -- Benchmark: scaling with depth (= number of partitions)
    let n = 1000000
    putStrLn "--- Benchmark: Spark parallelism scaling (N=1000000) ---"
    putStrLn "    depth=D means 2^D partitions, each sparked via parCombine"
    putStrLn ""

    printf "  %-8s  %-12s  %12s  %12s  %10s\n"
        ("Depth" :: String) ("Partitions" :: String)
        ("Sequential" :: String) ("Parallel" :: String)
        ("Speedup" :: String)

    -- Warmup
    _ <- benchSeq n 0
    _ <- benchPar n 0

    forM_ [0..6 :: Int] $ \depth -> do
        let parts = (2 :: Int) ^ depth
        -- Best of 5
        sTimes <- mapM (\_ -> benchSeq n depth) [1..5 :: Int]
        pTimes <- mapM (\_ -> benchPar n depth) [1..5 :: Int]
        let msS = minimum sTimes
            msP = minimum pTimes
            speedup = if msP > 0 then msS / msP else 0
        printf "  %-8d  %-12d  %9.1f ms  %9.1f ms  %8.2fx\n"
            depth parts msS msP speedup
    putStrLn ""

    -- Benchmark: scaling with array size at fixed depth
    putStrLn "--- Benchmark: Array size scaling (depth=3, 8 partitions) ---"
    putStrLn ""

    printf "  %-10s  %12s  %12s  %10s\n"
        ("N" :: String) ("Sequential" :: String)
        ("Parallel" :: String) ("Speedup" :: String)

    let depth2 = 3
    forM_ [10000, 100000, 1000000, 4000000] $ \nn -> do
        -- Warmup
        _ <- benchSeq nn depth2
        _ <- benchPar nn depth2

        -- Best of 5
        sTimes <- mapM (\_ -> benchSeq nn depth2) [1..5 :: Int]
        pTimes <- mapM (\_ -> benchPar nn depth2) [1..5 :: Int]
        let msS = minimum sTimes
            msP = minimum pTimes
            speedup = if msP > 0 then msS / msP else 0
        printf "  %-10d  %9.1f ms  %9.1f ms  %8.2fx\n"
            nn msS msP speedup
    putStrLn ""

    putStrLn "--- Summary ---"
    putStrLn "  parCombine uses GHC sparks (spark# + seq#) for parallel evaluation."
    putStrLn "  Linear types guarantee disjointness — no barriers, no races."
    putStrLn "  Same split/combine pattern as Demos 3-4, but parallelism is"
    putStrLn "  pure Haskell (GHC work-stealing) instead of C/OpenMP."
    putStrLn ""
    putStrLn "=== Done ==="
