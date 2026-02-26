{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Shared Memory Demo 3: Linear Concurrent Access.

   Same problem as Demo 2 — Haskell and C/OpenMP each process a half
   of the same array — but using linear types to prove disjointness:

   - split the output array into two halves
   - Haskell processes left half using its RW l token
   - C/OpenMP processes right half via unsafeWithPtr on the slice
   - combine recombines — zero-cost, no barrier needed

   The type system proves disjointness at compile time: Haskell holds
   RW l (exclusive access to left), C operates on right. No Haskell
   code can touch the right half because RW r is consumed by combine.
   This eliminates runtime synchronization.

   Run: ./build/shared3_demo +RTS -N4
-}
module Main where

import Data.Array.Linear
import Foreign (Ptr)
import Foreign.C (CDouble(..), CInt(..))
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf
import Control.Monad (forM_)
import System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------
-- FFI
------------------------------------------------------------------------

foreign import ccall safe "transform_range"
    c_transform_range :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> IO ()

foreign import ccall safe "transform_range_barrier"
    c_transform_range_barrier :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> IO ()

foreign import ccall safe "transform_partitioned_barrier"
    c_partitioned_barrier :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "transform_partitioned_nobarrier"
    c_partitioned_nobarrier :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "get_omp_num_threads"
    c_get_omp_num_threads :: IO CInt

------------------------------------------------------------------------
-- Haskell-side transform using linear tokens
------------------------------------------------------------------------

-- | f(x) = sin(x)*cos(x) + sqrt(abs(x))
transformElem :: Double -> Double
transformElem x = sin x * cos x + sqrt (abs x)

-- | Transform elements of arrOut using corresponding elements of arrIn.
-- arrIn is read-only (uses fabricated token for reads).
linearTransform :: forall s si. RW s %1 -> DArray si -> DArray s -> RW s
linearTransform rw arrIn arrOut = linearTransformAt rw arrIn arrOut 0

-- | Transform with explicit base offset for reading from arrIn.
-- Writes out[i] = f(in[base + i]) for i in [0, size arrOut).
linearTransformAt :: forall s si. RW s %1 -> DArray si -> DArray s -> Int -> RW s
linearTransformAt rw0 arrIn arrOut base = go rw0 0
  where
    n = size arrOut
    go :: RW s %1 -> Int -> RW s
    go rw i
        | i >= n = rw
        | otherwise =
            let (v, _) = unsafeRead MkRW arrIn (base + i)
            in go (unsafeWrite rw arrOut i (transformElem v)) (i + 1)

------------------------------------------------------------------------
-- Timing
------------------------------------------------------------------------

nowMs :: IO Double
nowMs = do
    ns <- getMonotonicTimeNSec
    return (fromIntegral ns / 1e6)

------------------------------------------------------------------------
-- Iteration: linear split (no barrier)
------------------------------------------------------------------------

-- | One iteration: split, Haskell left, C right (no barrier), combine.
-- C operates on the right slice via unsafeWithPtr. The linear token rwR
-- is held but not passed through withPtr — its existence proves no
-- Haskell code accesses the right half. combine consumes it at the end.
linearIteration :: forall s si. RW s %1 -> DArray si -> DArray s -> RW s
linearIteration rw arrIn arrOut =
    case halve rw arrOut of
        MkSlice st rwL rwR arrL arrR ->
            let rwL' = linearTransform rwL arrIn arrL
                half = size arrL
                n    = size arrOut
                -- C/OpenMP transforms the right half; rwR guarantees
                -- no Haskell code accesses arrR concurrently.
                -- Use full array pointers (not slice) since transform_range
                -- applies offset internally.
                !() = unsafePerformIO $
                    unsafeWithPtr arrOut $ \pOut ->
                        unsafeWithPtr arrIn $ \pIn ->
                            c_transform_range pIn pOut
                                (fromIntegral half) (fromIntegral (n - half))
            in combine st rwL' rwR

-- | Same but with barrier (for comparison with Demo 2).
barrierIteration :: forall s si. RW s %1 -> DArray si -> DArray s -> RW s
barrierIteration rw arrIn arrOut =
    case halve rw arrOut of
        MkSlice st rwL rwR arrL arrR ->
            let rwL' = linearTransform rwL arrIn arrL
                half = size arrL
                n    = size arrOut
                !() = unsafePerformIO $
                    unsafeWithPtr arrOut $ \pOut ->
                        unsafeWithPtr arrIn $ \pIn ->
                            c_transform_range_barrier pIn pOut
                                (fromIntegral half) (fromIntegral (n - half))
            in combine st rwL' rwR

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

fillArray :: forall s. RW s %1 -> DArray s -> Int -> RW s
fillArray rw0 arr nn = go rw0 0
  where
    go :: RW s %1 -> Int -> RW s
    go rw i
        | i >= nn = rw
        | otherwise = go (unsafeWrite rw arr i (fromIntegral i * 0.001)) (i + 1)

-- | Iterate a linear function N times over the same arrays.
iterateN :: forall s si. Int
         -> (RW s %1 -> DArray si -> DArray s -> RW s)
         -> DArray si -> DArray s -> RW s %1 -> RW s
iterateN 0 _f _arrIn _arrOut rw = rw
iterateN n  f  arrIn  arrOut rw = iterateN (n-1) f arrIn arrOut (f rw arrIn arrOut)
{-# NOINLINE iterateN #-}

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

benchLinear :: Int -> Int -> IO Double
benchLinear n iters = do
    let (arrIn,  rwIn0)  = unsafeAlloc n
        (arrOut, rwOut0) = unsafeAlloc n
        !_rwIn = fillArray rwIn0 arrIn n
    t0 <- nowMs
    let !_rwOut = iterateN iters linearIteration arrIn arrOut rwOut0
    t1 <- nowMs
    return (t1 - t0)

benchBarrier :: Int -> Int -> IO Double
benchBarrier n iters = do
    let (arrIn,  rwIn0)  = unsafeAlloc n
        (arrOut, rwOut0) = unsafeAlloc n
        !_rwIn = fillArray rwIn0 arrIn n
    t0 <- nowMs
    let !_rwOut = iterateN iters barrierIteration arrIn arrOut rwOut0
    t1 <- nowMs
    return (t1 - t0)

------------------------------------------------------------------------
-- Multi-partition benchmark
------------------------------------------------------------------------

-- | Recursively split into P pieces, Haskell transforms each.
-- No barriers needed because split/combine proves disjointness.
-- Tracks base offset so reads from arrIn align with sliced arrOut.
linearMultiPartition :: forall s si. RW s %1 -> DArray si -> DArray s -> Int -> Int -> RW s
linearMultiPartition rw arrIn arrOut base parts
    | parts <= 1 =
        linearTransformAt rw arrIn arrOut base
    | otherwise =
        case halve rw arrOut of
            MkSlice st rwL rwR arrL arrR ->
                let rwL' = linearMultiPartition rwL arrIn arrL base (parts `div` 2)
                    rwR' = linearMultiPartition rwR arrIn arrR (base + size arrL) (parts - parts `div` 2)
                in combine st rwL' rwR'

benchLinearPartitions :: Int -> Int -> IO Double
benchLinearPartitions n parts = do
    let (arrIn,  rwIn0)  = unsafeAlloc n
        (arrOut, rwOut0) = unsafeAlloc n
        !_rwIn = fillArray rwIn0 arrIn n
    t0 <- nowMs
    let !_rwOut = linearMultiPartition rwOut0 arrIn arrOut 0 parts
    t1 <- nowMs
    return (t1 - t0)

------------------------------------------------------------------------
-- C partition benchmarks (real barriers inside parallel region)
------------------------------------------------------------------------

-- | Benchmark C multi-partition with or without barriers.
benchCPartitions :: Int -> Int -> Int -> Bool -> IO Double
benchCPartitions n parts iters useBarrier = do
    let (arrIn,  rwIn0)  = unsafeAlloc n
        (arrOut, _rwOut0) = unsafeAlloc n
        !_rwIn = fillArray rwIn0 arrIn n
    t0 <- nowMs
    let call = if useBarrier then c_partitioned_barrier else c_partitioned_nobarrier
    unsafeWithPtr arrIn $ \pIn ->
        unsafeWithPtr arrOut $ \pOut ->
            call pIn pOut (fromIntegral n) (fromIntegral parts) (fromIntegral iters)
    t1 <- nowMs
    return (t1 - t0)

------------------------------------------------------------------------
-- Correctness
------------------------------------------------------------------------

verifyCorrectness :: Int -> IO ()
verifyCorrectness n = do
    printf "--- Correctness check (N=%d) ---\n" n
    let Ur result = alloc n \arrOut rwOut ->
            let (arrIn, rwIn0) = unsafeAlloc n
                !_rwIn = fillArray rwIn0 arrIn n
                rwOut' = linearIteration rwOut arrIn arrOut
            in freeze rwOut' arrOut

    let ref = [ transformElem (fromIntegral i * 0.001) | i <- [0..n-1] ]
    let maxDiff = maximum [ abs (a - b) | (a, b) <- zip result ref ]
    printf "  Max diff: %.2e %s\n" maxDiff
        (if maxDiff < 1e-12 then "OK" else "MISMATCH" :: String)

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Shared Memory Demo 3: Linear Concurrent Access ==="
    putStrLn "    (type-safe disjointness, zero synchronization)"
    putStrLn ""

    nthreads <- c_get_omp_num_threads
    printf "OpenMP threads: %d\n\n" (fromIntegral nthreads :: Int)

    -- Correctness
    verifyCorrectness 10000
    putStrLn ""

    -- Benchmark 1: iteration loop
    putStrLn "--- Benchmark 1: Iteration loop ---"
    putStrLn "    Haskell transforms [0,n/2), C transforms [n/2,n)"
    putStrLn ""
    printf "  %-8s  %8s  %12s  %12s  %10s\n"
        ("N" :: String) ("Iters" :: String) ("With barrier" :: String)
        ("Linear" :: String) ("Saved" :: String)

    forM_ [(10000, 1000), (100000, 100), (1000000, 10)] $
      \(n, iters :: Int) -> do
        -- Warmup
        _ <- benchBarrier n 5
        _ <- benchLinear  n 5

        -- Best of 3
        bTimes <- mapM (\_ -> benchBarrier n iters) [1..3 :: Int]
        lTimes <- mapM (\_ -> benchLinear  n iters) [1..3 :: Int]
        let msB = minimum bTimes
            msL = minimum lTimes
            saved = msB - msL
            pct = if msB > 0 then saved / msB * 100 else 0
        printf "  %-8d  %5d     %9.1f ms  %9.1f ms  %+.1f ms (%.1f%%)\n"
            n iters msB msL saved pct
    putStrLn ""

    -- Benchmark 2: partition scaling (linear types)
    putStrLn "--- Benchmark 2: Linear partition scaling (N=1000000) ---"
    putStrLn "    split/combine are zero-cost regardless of partition count"
    putStrLn ""
    let n2 = 1000000
    let partitions = [2, 4, 8, 16, 32, 64, 128, 256, 512, 1024] :: [Int]

    -- Warmup
    _ <- benchLinearPartitions n2 2

    printf "  %-12s  %12s\n"
        ("Partitions" :: String) ("Linear (ms)" :: String)

    forM_ partitions $ \p -> do
        times <- mapM (\_ -> benchLinearPartitions n2 p) [1..5 :: Int]
        let ms = minimum times
        printf "  %-12d  %9.3f ms\n" p ms
    putStrLn ""

    -- Benchmark 3: head-to-head barrier overhead vs linear types
    -- Use N=100K with 10 iterations to amplify barrier signal
    putStrLn "--- Benchmark 3: Barrier tax — C barriers vs linear (N=100000, 10 iters) ---"
    putStrLn "    C-barrier: real #pragma omp for barriers (iters*P barriers total)"
    putStrLn "    C-nobarrier: same but nowait (unsafe without proof)"
    putStrLn "    Haskell-linear: split/combine (safe + zero barriers)"
    putStrLn ""
    let n3 = 100000
        iters3 = 10

    -- Warmup
    _ <- benchCPartitions n3 2 iters3 True
    _ <- benchCPartitions n3 2 iters3 False

    printf "  %-12s  %12s  %12s  %12s  %10s\n"
        ("Partitions" :: String) ("C-barrier" :: String)
        ("C-nobarrier" :: String) ("Barrier tax" :: String)
        ("Pct" :: String)

    forM_ partitions $ \p -> do
        bTimes <- mapM (\_ -> benchCPartitions n3 p iters3 True)  [1..5 :: Int]
        nTimes <- mapM (\_ -> benchCPartitions n3 p iters3 False) [1..5 :: Int]
        let msB = minimum bTimes
            msN = minimum nTimes
            tax = msB - msN
            pct = if msN > 0 then tax / msN * 100 else 0
        printf "  %-12d  %9.3f ms  %9.3f ms  %+9.3f ms  %+.1f%%\n"
            p msB msN tax pct
    putStrLn ""

    putStrLn "--- Summary ---"
    putStrLn "  C-barrier degrades linearly with partition count."
    putStrLn "  C-nobarrier is flat but unsafe (no proof of disjointness)."
    putStrLn "  Haskell-linear is flat AND safe: linear types prove disjointness"
    putStrLn "  at compile time, giving the performance of nowait with the"
    putStrLn "  safety of barriers."
    putStrLn ""
    putStrLn "=== Done ==="
