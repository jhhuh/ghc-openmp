{- |
   Phase 15: Deferred OpenMP task execution.

   Tests that OpenMP tasks created with @#pragma omp task@ are deferred
   to a global queue and executed by idle threads (task stealing), rather
   than running inline on the creating thread.

   Run: ./task_demo +RTS -N4
-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Word (Word64)
import Foreign.C
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import Text.Printf

-- C function that creates NTASKS tasks inside a parallel single block
foreign import ccall safe "run_task_benchmark"
    c_run_task_benchmark :: CInt -> CInt -> IO CDouble

-- Sequential version for comparison
foreign import ccall unsafe "run_sequential_benchmark"
    c_run_sequential :: CInt -> CInt -> IO CDouble

foreign import ccall safe "get_omp_num_threads"
    c_get_omp_num_threads :: IO CInt

-- | Time an IO action, return nanoseconds.
timeIO :: IO a -> IO (a, Word64)
timeIO action = do
    t0 <- getMonotonicTimeNSec
    !result <- action
    t1 <- getMonotonicTimeNSec
    return (result, t1 - t0)

-- | Best-of-N timing.
bestOf :: Int -> IO a -> IO (a, Word64)
bestOf runs action = go runs undefined maxBound
  where
    go 0 bestResult bestTime = return (bestResult, bestTime)
    go n _ bestTime = do
        (result, elapsed) <- timeIO action
        if elapsed < bestTime
            then go (n - 1) result elapsed
            else go (n - 1) result bestTime

main :: IO ()
main = do
    putStrLn "=== Phase 15: Deferred Task Execution ==="
    putStrLn ""

    numCaps <- getNumCapabilities
    nthreads <- c_get_omp_num_threads
    printf "GHC Capabilities: %d\n" numCaps
    printf "OpenMP threads:   %d\n" (fromIntegral nthreads :: Int)
    putStrLn ""

    let workPerTask = 1000 :: Int
    let runs = 5

    -- Warmup
    _ <- c_run_sequential (100 :: CInt) (fromIntegral workPerTask)
    _ <- c_run_task_benchmark (100 :: CInt) (fromIntegral workPerTask)

    putStrLn "--- Task scaling: sequential vs deferred tasks ---"
    printf "  %-8s | %-12s | %-12s | %-8s\n"
        ("Tasks" :: String) ("Sequential" :: String)
        ("Parallel" :: String) ("Speedup" :: String)
    putStrLn "  ---------|--------------|--------------|----------"

    let taskCounts = [100, 500, 1000, 5000, 10000] :: [Int]

    mapM_ (\ntasks -> do
        let nt = fromIntegral ntasks :: CInt
        let wpt = fromIntegral workPerTask :: CInt

        (seqResult, seqNs) <- bestOf runs (c_run_sequential nt wpt)
        (parResult, parNs) <- bestOf runs (c_run_task_benchmark nt wpt)

        let seqMs = fromIntegral seqNs / 1e6 :: Double
        let parMs = fromIntegral parNs / 1e6 :: Double
        let speedup = seqMs / parMs

        printf "  %7d | %8.1f ms  | %8.1f ms  | %5.2fx\n"
            ntasks seqMs parMs speedup

        -- Verify correctness
        let seqD = realToFrac seqResult :: Double
        let parD = realToFrac parResult :: Double
        if abs (seqD - parD) > 1e-6
            then printf "  WARNING: results differ! seq=%.6f par=%.6f\n" seqD parD
            else return ()
        ) taskCounts

    putStrLn ""
    putStrLn "--- Analysis ---"
    putStrLn "  Tasks are created by one thread (#pragma omp single) and"
    putStrLn "  executed by all threads via task stealing in barriers."
    putStrLn "  Speedup scales with thread count for independent tasks."
    putStrLn ""
    putStrLn "=== Done ==="
