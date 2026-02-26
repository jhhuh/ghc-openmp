{-# LANGUAGE MagicHash, UnboxedTuples #-}
{- |
   Shared Memory Demo 2: Synchronized Concurrent Access.

   Same array, Haskell and OpenMP each process a half:
   - Haskell processes elements [0, n/2) sequentially
   - C/OpenMP processes elements [n/2, n) in parallel

   Without compile-time proof that the halves are disjoint, we use
   GOMP_barrier() between phases for memory visibility — even though
   the regions don't actually overlap. This is "defensive synchronization".

   Measures the barrier cost across many iterations and array sizes.
   Compare with Demo 3 which eliminates this cost via linear types.

   Run: ./build/shared2_demo +RTS -N4
-}
module Main where

import GHC.Exts
import GHC.IO (IO(..))
import Foreign (Ptr(..))
import Foreign.C (CDouble(..), CInt(..))
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf
import Control.Monad (forM_)

------------------------------------------------------------------------
-- FFI
------------------------------------------------------------------------

foreign import ccall safe "transform_range"
    c_transform_range :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> IO ()

foreign import ccall safe "transform_range_barrier"
    c_transform_range_barrier :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> IO ()

foreign import ccall safe "get_omp_num_threads"
    c_get_omp_num_threads :: IO CInt

------------------------------------------------------------------------
-- Pinned ByteArray utilities
------------------------------------------------------------------------

data PinnedDoubles = PinnedDoubles (MutableByteArray# RealWorld) Int

newPinnedDoubles :: Int -> IO PinnedDoubles
newPinnedDoubles (I# n) = IO $ \s0 ->
    case newPinnedByteArray# (n *# 8#) s0 of
        (# s1, mba #) -> (# s1, PinnedDoubles mba (I# n) #)

ptrOf :: PinnedDoubles -> Ptr CDouble
ptrOf (PinnedDoubles mba _) = Ptr (mutableByteArrayContents# mba)

touch :: PinnedDoubles -> IO ()
touch (PinnedDoubles mba _) = IO $ \s ->
    case touch# mba s of s' -> (# s', () #)

writeD :: PinnedDoubles -> Int -> Double -> IO ()
writeD (PinnedDoubles mba _) (I# i) (D# d) = IO $ \s ->
    case writeDoubleArray# mba i d s of s' -> (# s', () #)

readD :: PinnedDoubles -> Int -> IO Double
readD (PinnedDoubles mba _) (I# i) = IO $ \s ->
    case readDoubleArray# mba i s of (# s', d #) -> (# s', D# d #)

------------------------------------------------------------------------
-- Haskell-side transform
------------------------------------------------------------------------

transformElem :: Double -> Double
transformElem x = sin x * cos x + sqrt (abs x)

-- | Transform elements [off..off+len) in place
hsTransformRange :: PinnedDoubles -> PinnedDoubles -> Int -> Int -> IO ()
hsTransformRange arrIn arrOut off len =
    forM_ [off .. off + len - 1] $ \i -> do
        v <- readD arrIn i
        writeD arrOut i (transformElem v)

------------------------------------------------------------------------
-- Timing
------------------------------------------------------------------------

nowMs :: IO Double
nowMs = do
    ns <- getMonotonicTimeNSec
    return (fromIntegral ns / 1e6)

------------------------------------------------------------------------
-- Benchmark: iteration loop
------------------------------------------------------------------------

-- | Run iters iterations of: Haskell transforms [0,half), C transforms [half,n)
-- With barrier variant (defensive sync)
benchWithBarrier :: PinnedDoubles -> PinnedDoubles -> Int -> Int -> IO Double
benchWithBarrier arrIn arrOut n iters = do
    let half = n `div` 2
    t0 <- nowMs
    forM_ [1..iters] $ \_ -> do
        -- Haskell does left half
        hsTransformRange arrIn arrOut 0 half
        -- C/OpenMP does right half WITH barrier
        c_transform_range_barrier (ptrOf arrIn) (ptrOf arrOut)
            (fromIntegral half) (fromIntegral (n - half))
        touch arrIn; touch arrOut
    t1 <- nowMs
    return (t1 - t0)

-- | Same but without barrier (for comparison — still safe because
-- the two halves are disjoint, but a real program can't prove this)
benchWithoutBarrier :: PinnedDoubles -> PinnedDoubles -> Int -> Int -> IO Double
benchWithoutBarrier arrIn arrOut n iters = do
    let half = n `div` 2
    t0 <- nowMs
    forM_ [1..iters] $ \_ -> do
        hsTransformRange arrIn arrOut 0 half
        c_transform_range (ptrOf arrIn) (ptrOf arrOut)
            (fromIntegral half) (fromIntegral (n - half))
        touch arrIn; touch arrOut
    t1 <- nowMs
    return (t1 - t0)

------------------------------------------------------------------------
-- Benchmark: scaling by partition count
------------------------------------------------------------------------

-- | Split array into P chunks, each needing its own barrier.
-- Alternates: Haskell chunk, C chunk, Haskell chunk, C chunk, ...
benchPartitions :: PinnedDoubles -> PinnedDoubles -> Int -> Int -> Bool -> IO Double
benchPartitions arrIn arrOut n parts useBarrier = do
    let chunkSize = n `div` parts
    t0 <- nowMs
    let go _ 0 = return ()
        go off p = do
            let len = if p == 1 then n - off else chunkSize
            if even p
              then do
                -- Haskell chunk
                hsTransformRange arrIn arrOut off len
              else do
                -- C/OpenMP chunk
                if useBarrier
                  then c_transform_range_barrier (ptrOf arrIn) (ptrOf arrOut)
                           (fromIntegral off) (fromIntegral len)
                  else c_transform_range (ptrOf arrIn) (ptrOf arrOut)
                           (fromIntegral off) (fromIntegral len)
                touch arrIn; touch arrOut
            go (off + len) (p - 1)
    go 0 parts
    t1 <- nowMs
    return (t1 - t0)

------------------------------------------------------------------------
-- Correctness check
------------------------------------------------------------------------

verifyCorrectness :: Int -> IO ()
verifyCorrectness n = do
    printf "--- Correctness check (N=%d) ---\n" n
    arrIn  <- newPinnedDoubles n
    arrOut <- newPinnedDoubles n
    arrRef <- newPinnedDoubles n

    -- Fill input
    forM_ [0..n-1] $ \i -> writeD arrIn i (fromIntegral i * 0.001)

    -- Synchronized half-and-half
    let half = n `div` 2
    hsTransformRange arrIn arrOut 0 half
    c_transform_range_barrier (ptrOf arrIn) (ptrOf arrOut)
        (fromIntegral half) (fromIntegral (n - half))
    touch arrIn; touch arrOut

    -- Reference: Haskell-only
    forM_ [0..n-1] $ \i -> do
        v <- readD arrIn i
        writeD arrRef i (transformElem v)

    -- Compare
    maxDiff <- maxDiffArrays arrOut arrRef n
    printf "  Max diff: %.2e %s\n" maxDiff
        (if maxDiff < 1e-12 then "OK" else "MISMATCH" :: String)

maxDiffArrays :: PinnedDoubles -> PinnedDoubles -> Int -> IO Double
maxDiffArrays a b n = go 0 0.0
  where
    go i !mx
        | i >= n = return mx
        | otherwise = do
            va <- readD a i
            vb <- readD b i
            go (i + 1) (max mx (abs (va - vb)))

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Shared Memory Demo 2: Synchronized Concurrent Access ==="
    putStrLn "    (defensive barriers for memory visibility)"
    putStrLn ""

    nthreads <- c_get_omp_num_threads
    printf "OpenMP threads: %d\n\n" (fromIntegral nthreads :: Int)

    -- Correctness
    verifyCorrectness 10000
    putStrLn ""

    -- Benchmark 1: iteration loop at fixed array size
    putStrLn "--- Benchmark 1: Iteration loop (1000 iters) ---"
    putStrLn "    Haskell transforms [0,n/2), C transforms [n/2,n)"
    putStrLn ""
    printf "  %-8s  %12s  %12s  %10s\n"
        ("N" :: String) ("With barrier" :: String)
        ("No barrier" :: String) ("Overhead" :: String)

    forM_ [10000, 100000, 1000000] $ \n -> do
        arrIn  <- newPinnedDoubles n
        arrOut <- newPinnedDoubles n
        forM_ [0..n-1] $ \i -> writeD arrIn i (fromIntegral i * 0.001)

        -- Warmup
        _ <- benchWithBarrier arrIn arrOut n 10
        _ <- benchWithoutBarrier arrIn arrOut n 10

        let iters = if n <= 10000 then 1000 else if n <= 100000 then 100 else 10
        msBarrier   <- benchWithBarrier arrIn arrOut n iters
        msNoBarrier <- benchWithoutBarrier arrIn arrOut n iters

        let overhead = msBarrier - msNoBarrier
            pct = if msNoBarrier > 0 then overhead / msNoBarrier * 100 else 0
        printf "  %-8d  %9.1f ms  %9.1f ms  %+.1f ms (%.1f%%)\n"
            n msBarrier msNoBarrier overhead pct
    putStrLn ""

    -- Benchmark 2: scaling by partition count
    putStrLn "--- Benchmark 2: Scaling by partition count (N=1000000) ---"
    putStrLn "    More partitions = more barriers needed"
    putStrLn ""
    let n2 = 1000000
    arrIn2  <- newPinnedDoubles n2
    arrOut2 <- newPinnedDoubles n2
    forM_ [0..n2-1] $ \i -> writeD arrIn2 i (fromIntegral i * 0.001)

    -- Warmup
    _ <- benchPartitions arrIn2 arrOut2 n2 2 True
    _ <- benchPartitions arrIn2 arrOut2 n2 2 False

    printf "  %-12s  %12s  %12s  %10s\n"
        ("Partitions" :: String) ("With barrier" :: String)
        ("No barrier" :: String) ("Overhead" :: String)

    forM_ [2, 4, 8, 16, 32] $ \p -> do
        -- Best of 5
        bTimes <- mapM (\_ -> benchPartitions arrIn2 arrOut2 n2 p True)  [1..5 :: Int]
        nTimes <- mapM (\_ -> benchPartitions arrIn2 arrOut2 n2 p False) [1..5 :: Int]
        let msB = minimum bTimes
            msN = minimum nTimes
            overhead = msB - msN
        printf "  %-12d  %9.3f ms  %9.3f ms  %+.3f ms\n"
            p msB msN overhead
    putStrLn ""

    putStrLn "--- Summary ---"
    putStrLn "  Without compile-time proof of disjointness, correct programs"
    putStrLn "  must include barriers for memory visibility."
    putStrLn "  Barrier cost accumulates with iterations and partition count."
    putStrLn "  Demo 3 eliminates this with linear types."
    putStrLn ""
    putStrLn "=== Done ==="
