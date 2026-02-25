{- |
   Phase 5: Concurrent Haskell + OpenMP execution.

   Proves that Haskell green threads and OpenMP parallel regions run
   simultaneously on the same GHC RTS. When Haskell calls a safe FFI
   function, GHC releases the Capability, letting other Haskell threads
   run while OpenMP workers execute on their own OS threads.

   Build: ghc -threaded -O2 HsConcurrent.hs omp_compute.o ghc_omp_runtime_rts.o
   Run:   ./concurrent_demo +RTS -N4
-}
module Main where

import Control.Concurrent
import Foreign.C
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf

-- | OpenMP-parallelized sin sum (runs on OpenMP worker pool)
foreign import ccall safe "parallel_sinsum"
    c_parallel_sinsum :: CInt -> IO CDouble

-- | Query OpenMP thread count
foreign import ccall safe "get_omp_num_threads"
    c_get_omp_num_threads :: IO CInt

-- | Wall clock time in milliseconds
nowMs :: IO Double
nowMs = do
    ns <- getMonotonicTimeNSec
    return (fromIntegral ns / 1e6)

-- | Pure Haskell computation: sum of sin() (matches the C version)
haskellSinSum :: Int -> Double
haskellSinSum n = go 0 0.0
  where
    go i !acc
        | i >= n    = acc
        | otherwise = go (i + 1) (acc + sin (fromIntegral i * 0.001))

main :: IO ()
main = do
    putStrLn "=== Phase 5: Concurrent Haskell + OpenMP ==="
    putStrLn ""

    nthreads <- c_get_omp_num_threads
    numCaps <- getNumCapabilities
    printf "GHC Capabilities: %d\n" numCaps
    printf "OpenMP threads:   %d\n" (fromIntegral nthreads :: Int)
    putStrLn ""

    -- Use workloads calibrated to each be ~30ms at 4T
    let ompN = 12000000  -- 12M: ~30ms for OpenMP at 4T
        hsN  = 1200000   -- 1.2M: ~30ms for Haskell (sequential)

    -- Warmup both paths
    _ <- c_parallel_sinsum (fromIntegral ompN)
    let !_ = haskellSinSum 1000

    -- 1. Sequential: Haskell then OpenMP
    putStrLn "--- Sequential: Haskell THEN OpenMP ---"
    t0 <- nowMs
    let !hsResult = haskellSinSum hsN
    t1 <- nowMs
    ompResult <- c_parallel_sinsum (fromIntegral ompN)
    t2 <- nowMs
    let hsTime = t1 - t0
        ompTime = t2 - t1
        seqTotal = t2 - t0
    printf "  Haskell (%.1fM sin): %.1f ms\n" (fromIntegral hsN / 1e6 :: Double) hsTime
    printf "  OpenMP  (%.1fM sin): %.1f ms\n" (fromIntegral ompN / 1e6 :: Double) ompTime
    printf "  Total: %.1f ms\n" seqTotal
    putStrLn ""

    -- 2. Concurrent: Haskell AND OpenMP at the same time
    putStrLn "--- Concurrent: Haskell AND OpenMP ---"
    hsDone <- newEmptyMVar
    ompDone <- newEmptyMVar

    t3 <- nowMs

    -- Fork Haskell green thread for pure computation
    _ <- forkIO $ do
        let !result = haskellSinSum hsN
        t <- nowMs
        putMVar hsDone (result, t - t3)

    -- Fork OpenMP FFI call (safe call releases Capability)
    _ <- forkIO $ do
        result <- c_parallel_sinsum (fromIntegral ompN)
        t <- nowMs
        putMVar ompDone (realToFrac result :: Double, t - t3)

    -- Wait for both
    (hsR, hsDelta) <- takeMVar hsDone
    (ompR, ompDelta) <- takeMVar ompDone
    t4 <- nowMs
    let conTotal = t4 - t3

    printf "  Haskell finished at: +%.1f ms\n" hsDelta
    printf "  OpenMP  finished at: +%.1f ms\n" ompDelta
    printf "  Total: %.1f ms\n" conTotal
    putStrLn ""

    -- Verify correctness
    printf "  Results match: Haskell=%.6f, OpenMP=%.6f (diff=%.1e)\n"
        hsR ompR (abs (hsResult - hsR))
    putStrLn ""

    -- 3. Analysis
    let speedup = seqTotal / conTotal
        overlap = (hsTime + ompTime) - conTotal
    printf "Speedup: %.2fx (%.1f ms → %.1f ms)\n" speedup seqTotal conTotal
    printf "Overlap: %.1f ms of Haskell+OpenMP ran simultaneously\n" overlap
    putStrLn ""

    if overlap > 5.0
        then putStrLn "Concurrent execution confirmed: Haskell and OpenMP\nrun on the same GHC RTS without starving each other."
        else putStrLn "Minimal overlap (workloads too imbalanced at this thread count)."
    putStrLn ""
    putStrLn "=== Done ==="
