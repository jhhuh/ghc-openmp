{- |
   Phase 14: GHC native parallelism vs OpenMP.

   Compares three approaches for the same compute-bound workload (sinsum):
     1. Sequential Haskell (single-threaded baseline)
     2. Parallel Haskell (forkIO + manual work splitting)
     3. OpenMP via safe FFI (our RTS-backed runtime)

   Both Haskell and OpenMP run on the same GHC RTS thread pool.

   Run: ./par_compare +RTS -N4
-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import Data.IORef
import Data.Word (Word64)
import Foreign.C
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import Text.Printf

-- OpenMP-parallelized sinsum via safe FFI
foreign import ccall safe "parallel_sinsum"
    c_parallel_sinsum :: CInt -> IO CDouble

-- Sequential C sinsum via unsafe FFI
foreign import ccall unsafe "sequential_sinsum"
    c_sequential_sinsum :: CInt -> IO CDouble

foreign import ccall safe "get_omp_num_threads"
    c_get_omp_num_threads :: IO CInt

-- | Sequential Haskell sinsum.
{-# NOINLINE haskellSinSum #-}
haskellSinSum :: Int -> Int -> Double
haskellSinSum lo hi = go lo 0.0
  where
    go !i !acc
        | i >= hi   = acc
        | otherwise = go (i + 1) (acc + sin (fromIntegral i * 0.001))

-- | Parallel Haskell sinsum using forkIO + MVars.
-- Splits work evenly across N capabilities.
{-# NOINLINE parallelHaskellSinSum #-}
parallelHaskellSinSum :: Int -> Int -> IO Double
parallelHaskellSinSum numThreads n = do
    let chunkSize = n `div` numThreads
    results <- forM [0 .. numThreads - 1] $ \tid -> do
        mv <- newEmptyMVar
        let lo = tid * chunkSize
            hi = if tid == numThreads - 1 then n else (tid + 1) * chunkSize
        _ <- forkIO $ do
            let !partial = haskellSinSum lo hi
            putMVar mv partial
        return mv
    partials <- mapM takeMVar results
    return (sum partials)

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
    putStrLn "=== Phase 14: GHC Native Parallelism vs OpenMP ==="
    putStrLn ""

    numCaps <- getNumCapabilities
    nthreads <- c_get_omp_num_threads
    printf "GHC Capabilities: %d\n" numCaps
    printf "OpenMP threads:   %d\n" (fromIntegral nthreads :: Int)
    putStrLn ""

    -- Warmup all paths
    let !_ = haskellSinSum 0 1000
    _ <- parallelHaskellSinSum numCaps 1000
    _ <- c_parallel_sinsum 1000
    _ <- c_sequential_sinsum 1000

    let sizes = [1000, 5000, 10000, 50000, 100000, 500000,
                 1000000, 5000000, 10000000] :: [Int]
    let runs = 5

    putStrLn "--- sinsum: sequential Haskell vs parallel Haskell vs OpenMP ---"
    putStrLn ""
    printf "  %-10s | %-12s | %-12s | %-12s | %-12s | %-7s | %-7s\n"
        ("Elements" :: String) ("Seq Haskell" :: String) ("Seq C" :: String)
        ("Par Haskell" :: String) ("Par OpenMP" :: String)
        ("Hs/OMP" :: String) ("C/OMP" :: String)
    putStrLn "  -----------|--------------|--------------|--------------|--------------|---------|--------"

    -- Use IORefs to prevent GHC from lifting pure computations to CAFs
    sinkRef <- newIORef (0.0 :: Double)
    szRef   <- newIORef (0 :: Int)

    forM_ sizes $ \sz -> do
        let n = fromIntegral sz :: CInt
        writeIORef szRef sz

        -- Sequential Haskell: read size from IORef to prevent CAF sharing
        (_, seqHsNs) <- bestOf runs $ do
            s <- readIORef szRef
            !r <- evaluate (haskellSinSum 0 s)
            writeIORef sinkRef r

        -- Sequential C (unsafe FFI)
        (_, seqCNs) <- bestOf runs $ do
            !r <- c_sequential_sinsum n
            writeIORef sinkRef (realToFrac r)

        -- Parallel Haskell (forkIO)
        (_, parHsNs) <- bestOf runs $ do
            !r <- parallelHaskellSinSum numCaps sz
            writeIORef sinkRef r

        -- Parallel OpenMP (safe FFI)
        (_, parOmpNs) <- bestOf runs $ do
            !r <- c_parallel_sinsum n
            writeIORef sinkRef (realToFrac r)

        let seqHsUs  = fromIntegral seqHsNs / 1000.0 :: Double
        let seqCUs   = fromIntegral seqCNs / 1000.0 :: Double
        let parHsUs  = fromIntegral parHsNs / 1000.0 :: Double
        let parOmpUs = fromIntegral parOmpNs / 1000.0 :: Double
        let hsOmpRatio = parHsUs / parOmpUs
        let cOmpRatio  = seqCUs / parOmpUs

        printf "  %9d | %9.1f us | %9.1f us | %9.1f us | %9.1f us | %5.2fx  | %5.2fx\n"
            sz seqHsUs seqCUs parHsUs parOmpUs hsOmpRatio cOmpRatio

    putStrLn ""

    -- Verify correctness
    putStrLn "--- Correctness verification ---"
    let n = 100000
    let hsRef = haskellSinSum 0 n
    parHsResult <- parallelHaskellSinSum numCaps n
    ompResult <- c_parallel_sinsum (fromIntegral n)
    seqCResult <- c_sequential_sinsum (fromIntegral n)
    let ompD = realToFrac ompResult :: Double
    let seqCD = realToFrac seqCResult :: Double

    printf "  Sequential Haskell: %.10f\n" hsRef
    printf "  Parallel Haskell:   %.10f (diff = %.1e)\n" parHsResult (abs (parHsResult - hsRef))
    printf "  Sequential C:       %.10f (diff = %.1e)\n" seqCD (abs (seqCD - hsRef))
    printf "  Parallel OpenMP:    %.10f (diff = %.1e)\n" ompD (abs (ompD - hsRef))
    putStrLn ""

    -- Analysis
    putStrLn "--- Analysis ---"
    putStrLn "  OpenMP (C) advantages:"
    putStrLn "    - Unboxed computation: no allocation, no GC pressure"
    putStrLn "    - GCC vectorization: SIMD for numeric loops"
    putStrLn "    - Cache-friendly: contiguous memory, no thunk indirection"
    putStrLn ""
    putStrLn "  Haskell forkIO advantages:"
    putStrLn "    - No FFI boundary: works on Haskell data structures directly"
    putStrLn "    - Lightweight: thousands of green threads at low cost"
    putStrLn "    - Composable: integrates with STM, async, streaming"
    putStrLn ""
    putStrLn "  Guideline: use OpenMP for unboxed numeric arrays,"
    putStrLn "  Haskell parallelism for functional transformations."
    putStrLn ""
    putStrLn "=== Done ==="
