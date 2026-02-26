{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Shared Memory Demo 4: Safety — Why barriers exist.

   Two examples showing that removing barriers (nowait) without
   proof of disjointness silently introduces bugs:

   Part A: Off-by-one overlap
     Partitions overlap by 1 element. With nowait, boundary elements
     are written by two threads simultaneously — a data race.
     With linear types, split produces non-overlapping views and the
     type system rejects overlapping access.

   Part B: Two-pass stencil
     Pass 1 writes out[i], pass 2 reads out[i-1]/out[i+1].
     Without a barrier between passes, pass 2 reads stale data
     at partition boundaries. With linear types, combine forces
     pass 1 to complete before pass 2 can read across boundaries.

   Run: ./build/shared4_demo +RTS -N4
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

-- Correct versions (disjoint partitions)
foreign import ccall safe "transform_partitioned_barrier"
    c_partitioned_barrier :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "transform_partitioned_nobarrier"
    c_partitioned_nobarrier :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> IO ()

-- Overlap bug (Demo 4A)
foreign import ccall safe "transform_overlap_nowait"
    c_overlap_nowait :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> IO ()

foreign import ccall safe "transform_overlap_barrier"
    c_overlap_barrier :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> IO ()

-- Stencil (Demo 4B)
foreign import ccall safe "stencil_barrier"
    c_stencil_barrier :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> IO ()

foreign import ccall safe "stencil_nowait"
    c_stencil_nowait :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> IO ()

foreign import ccall safe "get_omp_num_threads"
    c_get_omp_num_threads :: IO CInt

------------------------------------------------------------------------
-- Haskell-side transforms (using linear types)
------------------------------------------------------------------------

transformElem :: Double -> Double
transformElem x = sin x * cos x + sqrt (abs x)

-- | Transform a partition: out[i] = f(in[i]).
linearTransform :: forall s si. RW s %1 -> DArray si -> DArray s -> RW s
linearTransform rw0 arrIn arrOut = linearTransformAt rw0 arrIn arrOut 0

-- | Transform a partition with explicit base offset for reading input.
-- Writes out[i] = f(in[base + i]) for i in [0, size arrOut).
-- When arrOut is a slice with offset, base should equal the global offset
-- so that reads from arrIn align with writes to arrOut.
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

-- | Two-pass stencil using linear types.
-- Pass 1: write out[i] = f(in[i]) in disjoint partitions.
-- combine forces all partitions to complete before pass 2.
-- Pass 2: out[i] = (out[i-1] + out[i] + out[i+1]) / 3.0
-- requires parent token because it reads across partition boundaries.
linearStencil :: forall s si. RW s %1 -> DArray si -> DArray s -> Int -> RW s
linearStencil rw arrIn arrOut numParts =
    let -- Pass 1: partitioned writes (can be split)
        rw1 = linearMultiPass1 rw arrIn arrOut 0 numParts
        -- combine happened inside linearMultiPass1 — rw1 is the parent token
        -- Pass 2: stencil needs cross-boundary reads → requires parent token
    in  linearPass2 rw1 arrOut

-- | Pass 1: recursively split, transform each partition, recombine.
-- Tracks base offset so reads from arrIn align with sliced arrOut.
linearMultiPass1 :: forall s si. RW s %1 -> DArray si -> DArray s -> Int -> Int -> RW s
linearMultiPass1 rw arrIn arrOut base parts
    | parts <= 1 = linearTransformAt rw arrIn arrOut base
    | otherwise =
        case halve rw arrOut of
            MkSlice st rwL rwR arrL arrR ->
                let rwL' = linearMultiPass1 rwL arrIn arrL base (parts `div` 2)
                    rwR' = linearMultiPass1 rwR arrIn arrR (base + size arrL) (parts - parts `div` 2)
                in combine st rwL' rwR'

-- | Pass 2: 3-point stencil averaging. Needs parent token because it reads
-- elements across partition boundaries (out[i-1], out[i+1]).
linearPass2 :: forall s. RW s %1 -> DArray s -> RW s
linearPass2 rw0 arr = go rw0 0
  where
    n = size arr
    go :: RW s %1 -> Int -> RW s
    go rw i =
        if i >= n then rw
        else
            let (cur, _)   = unsafeRead MkRW arr i
                (left, _)  = if i > 0     then unsafeRead MkRW arr (i - 1) else (cur, MkRW)
                (right, _) = if i < n - 1 then unsafeRead MkRW arr (i + 1) else (cur, MkRW)
            in go (unsafeWrite rw arr i ((left + cur + right) / 3.0)) (i + 1)

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

-- | Compute reference result: single-threaded, no partitioning.
referenceTransform :: forall s si. RW s %1 -> DArray si -> DArray s -> RW s
referenceTransform = linearTransform

-- | Compute reference stencil: single-threaded, two passes.
referenceStencil :: forall s si. RW s %1 -> DArray si -> DArray s -> RW s
referenceStencil rw arrIn arrOut =
    let rw1 = linearTransform rw arrIn arrOut
    in  linearPass2 rw1 arrOut

-- | Max absolute difference between two arrays.
-- Uses fabricated tokens (MkRW) for reads — safe for verification.
maxDiffArrays :: DArray s1 -> DArray s2 -> Int -> Double
maxDiffArrays arr1 arr2 n = go 0 0.0
  where
    go i !mx
        | i >= n = mx
        | otherwise =
            let (v1, _) = unsafeRead MkRW arr1 i
                (v2, _) = unsafeRead MkRW arr2 i
            in go (i + 1) (max mx (abs (v1 - v2)))

------------------------------------------------------------------------
-- Part A: Overlap bug
------------------------------------------------------------------------

runOverlapTest :: Int -> Int -> IO ()
runOverlapTest n numParts = do
    printf "  N=%d, P=%d partitions:\n" n numParts

    -- Compute reference (correct, single-threaded)
    let (arrIn, rwIn0) = unsafeAlloc n
        !_rwIn = fillArray rwIn0 arrIn n
        (refOut, rwRef0) = unsafeAlloc n
        !_rwRef = referenceTransform rwRef0 arrIn refOut

    -- C disjoint + nobarrier (correct)
    let (disjOut, _rwD) = unsafeAlloc n
    unsafeWithPtr arrIn $ \pIn ->
        unsafeWithPtr disjOut $ \pOut ->
            c_partitioned_nobarrier pIn pOut (fromIntegral n) (fromIntegral numParts) 1

    -- C overlap + barrier (deterministic but wrong)
    let (obOut, _rwOB) = unsafeAlloc n
    unsafeWithPtr arrIn $ \pIn ->
        unsafeWithPtr obOut $ \pOut ->
            c_overlap_barrier pIn pOut (fromIntegral n) (fromIntegral numParts)

    -- C overlap + nowait (data race)
    let (onOut, _rwON) = unsafeAlloc n
    unsafeWithPtr arrIn $ \pIn ->
        unsafeWithPtr onOut $ \pOut ->
            c_overlap_nowait pIn pOut (fromIntegral n) (fromIntegral numParts)

    -- Compare all against reference
    let diffDisj = maxDiffArrays disjOut refOut n
    let diffOB   = maxDiffArrays obOut   refOut n
    let diffON   = maxDiffArrays onOut   refOut n

    printf "    Disjoint + nobarrier:  max diff = %.2e  %s\n"
        diffDisj (if diffDisj < 1e-12 then "CORRECT" else "ERROR" :: String)
    printf "    Overlap  + barrier:    max diff = %.2e  %s\n"
        diffOB (if diffOB < 1e-12 then "correct" else "WRONG (double-write at boundaries)" :: String)
    printf "    Overlap  + nowait:     max diff = %.2e  %s\n"
        diffON (if diffON < 1e-12 then "correct" else "WRONG (data race at boundaries)" :: String)

------------------------------------------------------------------------
-- Part B: Two-pass stencil
------------------------------------------------------------------------

runStencilTest :: Int -> Int -> IO ()
runStencilTest n numParts = do
    printf "  N=%d, P=%d partitions:\n" n numParts

    let (arrIn, rwIn0) = unsafeAlloc n
        !_rwIn = fillArray rwIn0 arrIn n

    -- C stencil + barrier (correct — Jacobi reference)
    let (barOut, _rwB) = unsafeAlloc n
    unsafeWithPtr arrIn $ \pIn ->
        unsafeWithPtr barOut $ \pOut ->
            c_stencil_barrier pIn pOut (fromIntegral n) (fromIntegral numParts)

    -- C stencil + nowait (data race) — run multiple times, take worst diff
    maxNwDiff <- fmap maximum $ mapM (\_ -> do
        let (nwOut, _rwN) = unsafeAlloc n
        unsafeWithPtr arrIn $ \pIn ->
            unsafeWithPtr nwOut $ \pOut ->
                c_stencil_nowait pIn pOut (fromIntegral n) (fromIntegral numParts)
        return (maxDiffArrays nwOut barOut n)
        ) [1..10 :: Int]

    -- Haskell reference: sequential two-pass stencil
    let (refOut, rwRef0) = unsafeAlloc n
        !_rwRef = referenceStencil rwRef0 arrIn refOut

    -- Haskell linear stencil (correct — combine enforces ordering)
    let (linOut, rwLin0) = unsafeAlloc n
        !_rwLin = linearStencil rwLin0 arrIn linOut numParts

    -- Compare C nowait against C barrier (same Jacobi algorithm)
    -- Compare Haskell linear against Haskell reference (same Gauss-Seidel)
    let diffLin = maxDiffArrays linOut refOut n

    printf "    C barrier:             reference (correct Jacobi stencil)\n"
    printf "    C nowait (10 runs):    max diff vs barrier = %.2e  %s\n"
        maxNwDiff (if maxNwDiff < 1e-12 then "correct (lucky timing)" else "WRONG (stale reads)" :: String)
    printf "    Haskell linear:        max diff vs ref     = %.2e  %s\n"
        diffLin (if diffLin < 1e-12 then "CORRECT" else "ERROR" :: String)

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Shared Memory Demo 4: Safety ==="
    putStrLn "    Why barriers exist, and how linear types replace them"
    putStrLn ""

    nthreads <- c_get_omp_num_threads
    printf "OpenMP threads: %d\n\n" (fromIntegral nthreads :: Int)

    -- Part A: Overlap bug
    putStrLn "--- Part A: Off-by-one overlap ---"
    putStrLn "    Each partition writes [off..off+chunk+1) instead of [off..off+chunk)."
    putStrLn "    Boundary elements are written by two partitions."
    putStrLn "    With nowait: data race (non-deterministic)."
    putStrLn "    With barrier: deterministic but wrong (double-write)."
    putStrLn "    With linear split: impossible — type rejects overlapping ranges."
    putStrLn ""

    forM_ [(10000, 4), (100000, 16), (1000000, 64)] $ \(n, p) ->
        runOverlapTest n p
    putStrLn ""

    -- Part B: Two-pass stencil
    putStrLn "--- Part B: Two-pass stencil ---"
    putStrLn "    Pass 1: out[i] = f(in[i])              (independent)"
    putStrLn "    Pass 2: out[i] = avg(out[i-1..i+1])    (reads neighbors)"
    putStrLn "    Without barrier: pass 2 reads stale data at boundaries."
    putStrLn "    With linear types: combine forces pass 1 completion before pass 2."
    putStrLn ""

    forM_ [(10000, 4), (100000, 16), (1000000, 64)] $ \(n, p) ->
        runStencilTest n p
    putStrLn ""

    putStrLn "--- Summary ---"
    putStrLn "  Removing barriers (nowait) without proof of disjointness is unsafe:"
    putStrLn "  - Overlap bugs cause data races (Part A)"
    putStrLn "  - Missing barriers cause stale reads (Part B)"
    putStrLn "  Linear types prevent both at compile time:"
    putStrLn "  - split produces non-overlapping views (prevents Part A)"
    putStrLn "  - combine forces completion before cross-boundary reads (prevents Part B)"
    putStrLn ""
    putStrLn "=== Done ==="
