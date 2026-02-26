{-# LANGUAGE MagicHash, UnboxedTuples #-}
{- |
   Shared Memory Demo 1: Producer-Consumer (sequential handoff).

   Demonstrates zero-copy sharing between Haskell and OpenMP C via
   pinned ByteArray:

   1. Haskell allocates a pinned array of N doubles
   2. Haskell fills it with input data (producer)
   3. Passes raw Addr# to C via safe FFI
   4. C/OpenMP transforms all elements in parallel
   5. Haskell reads results back and sums them (consumer)

   No concurrent access — clean sequential phases.

   Run: ./build/shared1_demo +RTS -N4
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

foreign import ccall safe "transform_all"
    c_transform_all :: Ptr CDouble -> Ptr CDouble -> CInt -> IO ()

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
-- Haskell-side transform (sequential reference)
------------------------------------------------------------------------

-- | Same function as C: f(x) = sin(x)*cos(x) + sqrt(abs(x))
transformElem :: Double -> Double
transformElem x = sin x * cos x + sqrt (abs x)

------------------------------------------------------------------------
-- Timing
------------------------------------------------------------------------

nowMs :: IO Double
nowMs = do
    ns <- getMonotonicTimeNSec
    return (fromIntegral ns / 1e6)

bestOf :: Int -> IO a -> IO (a, Double)
bestOf runs action = do
    r <- action  -- warmup
    times <- mapM (\_ -> do
        t0 <- nowMs
        _ <- action
        t1 <- nowMs
        return (t1 - t0)) [1..runs]
    return (r, minimum times)

------------------------------------------------------------------------
-- Demo
------------------------------------------------------------------------

runDemo :: Int -> IO ()
runDemo n = do
    printf "--- N = %d ---\n" n

    -- Allocate input and output arrays
    arrIn  <- newPinnedDoubles n
    arrOut <- newPinnedDoubles n

    -- Phase 1: Haskell fills input (producer)
    forM_ [0..n-1] $ \i ->
        writeD arrIn i (fromIntegral i * 0.001)

    -- Phase 2: C/OpenMP transforms all elements in parallel
    (_, ompMs) <- bestOf 5 $ do
        c_transform_all (ptrOf arrIn) (ptrOf arrOut) (fromIntegral n)
        touch arrIn; touch arrOut

    -- Phase 3: Haskell reads results (consumer) and sums
    s <- sumArray arrOut n

    -- Correctness: compare against Haskell sequential reference
    refOut <- newPinnedDoubles n
    forM_ [0..n-1] $ \i -> do
        v <- readD arrIn i
        writeD refOut i (transformElem v)
    maxDiff <- maxDiffArrays arrOut refOut n

    printf "  OpenMP transform: %.3f ms\n" ompMs
    printf "  Sum of output:    %.6f\n" s
    printf "  Max diff vs ref:  %.2e %s\n" maxDiff
        (if maxDiff < 1e-12 then "OK" else "MISMATCH" :: String)

sumArray :: PinnedDoubles -> Int -> IO Double
sumArray arr n = go 0 0.0
  where
    go i !acc
        | i >= n = return acc
        | otherwise = do
            v <- readD arr i
            go (i + 1) (acc + v)

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
    putStrLn "=== Shared Memory Demo 1: Producer-Consumer ==="
    putStrLn "    (sequential handoff via pinned ByteArray)"
    putStrLn ""

    nthreads <- c_get_omp_num_threads
    printf "OpenMP threads: %d\n\n" (fromIntegral nthreads :: Int)

    putStrLn "Pattern: Haskell fills array -> C/OpenMP transforms -> Haskell reads"
    putStrLn "No concurrent access, clean sequential phases."
    putStrLn ""

    runDemo 10000
    putStrLn ""
    runDemo 100000
    putStrLn ""
    runDemo 1000000
    putStrLn ""

    putStrLn "--- Summary ---"
    putStrLn "  Zero-copy: pinned ByteArray# shared directly with C"
    putStrLn "  No marshalling: mutableByteArrayContents# gives raw Addr#"
    putStrLn "  Sequential phases: no synchronization needed"
    putStrLn ""
    putStrLn "=== Done ==="
