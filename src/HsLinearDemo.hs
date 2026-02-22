{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Phase 17: Linear typed arrays — type-safe parallel sub-array access.

   Demonstrates:
   1. Zero-copy C FFI via withPtr (same OpenMP DGEMM kernel)
   2. Type-safe split/combine: disjoint row partitioning of output matrix
   3. Compile-time prevention of aliased writes
   4. No external dependencies (self-contained linear array module)

   Run: ./linear_demo +RTS -N4
-}
module Main where

import Data.Array.Linear
import Foreign (Ptr)
import Foreign.C (CDouble(..), CInt(..))
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf

-- FFI imports (same C kernels as Phase 7/16)
foreign import ccall safe "parallel_dgemm"
    c_parallel_dgemm :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO ()

foreign import ccall safe "get_omp_num_threads"
    c_get_omp_num_threads :: IO CInt

-- | Wall clock time in milliseconds
nowMs :: IO Double
nowMs = do
    ns <- getMonotonicTimeNSec
    return (fromIntegral ns / 1e6)

------------------------------------------------------------------------
-- Matrix helpers using linear arrays
------------------------------------------------------------------------

-- | Fill a matrix (rows x cols) with a function, consuming and
-- returning the token.
fillMatrix :: forall s. RW s %1 -> DArray s -> Int -> Int
           -> (Int -> Int -> Double) -> RW s
fillMatrix rw0 arr rows cols f = goI rw0 0
  where
    goI :: RW s %1 -> Int -> RW s
    goI rw i
        | i >= rows = rw
        | otherwise = goI (goJ rw i 0) (i + 1)
    goJ :: RW s %1 -> Int -> Int -> RW s
    goJ rw i j
        | j >= cols = rw
        | otherwise = goJ (unsafeWrite rw arr (i * cols + j) (f i j)) i (j + 1)

-- | Sequential DGEMM on linear arrays.
-- Computes C[0..rowsLocal-1, :] = A[rowOff..rowOff+rowsLocal-1, :] * B
linearDgemm :: forall s sa sb. RW s %1 -> Int -> Int -> Int
            -> DArray sa -> DArray sb -> DArray s -> RW s
linearDgemm rwC0 n rowOff rowsLocal arrA arrB arrC = goI rwC0 0
  where
    goI :: RW s %1 -> Int -> RW s
    goI rw i
        | i >= rowsLocal = rw
        | otherwise = goI (goJ rw i 0) (i + 1)
    goJ :: RW s %1 -> Int -> Int -> RW s
    goJ rw i j
        | j >= n = rw
        | otherwise =
            let !s = goK 0 0.0
            in  goJ (unsafeWrite rw arrC (i * n + j) s) i (j + 1)
      where
        goK :: Int -> Double -> Double
        goK k !acc
            | k >= n    = acc
            | otherwise =
                let (a, _) = readRO arrA ((rowOff + i) * n + k)
                    (b, _) = readRO arrB (k * n + j)
                in  goK (k + 1) (acc + a * b)

-- | Read-only access. Fabricates a token — safe when the array is
-- only ever read (A and B in DGEMM).
readRO :: DArray s -> Int -> (Double, ())
readRO arr i = case unsafeRead MkRW arr i of (val, _) -> (val, ())
{-# INLINE readRO #-}

-- | Read-only withPtr wrapper using unsafeWithPtr from the module.
withPtrRO :: DArray s -> (Ptr CDouble -> IO a) -> IO a
withPtrRO = unsafeWithPtr

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

-- | Test 1: Basic alloc / write / read / freeze
test1_basic :: IO ()
test1_basic = do
    putStrLn "--- Test 1: Basic alloc / write / read / freeze ---"
    let Ur result = alloc 5 \arr rw ->
            let rw1 = unsafeWrite rw  arr 0 10.0
                rw2 = unsafeWrite rw1 arr 1 20.0
                rw3 = unsafeWrite rw2 arr 2 30.0
                rw4 = unsafeWrite rw3 arr 3 40.0
                rw5 = unsafeWrite rw4 arr 4 50.0
            in  freeze rw5 arr
    printf "  Written [10,20,30,40,50], read back: %s\n"
        (show result)
    let ok = result == [10.0, 20.0, 30.0, 40.0, 50.0]
    printf "  %s\n" (if ok then "OK" else "MISMATCH" :: String)

-- | Test 2: Split / combine (zero-copy)
test2_split :: IO ()
test2_split = do
    putStrLn "--- Test 2: Split / combine (zero-copy) ---"
    let Ur result = alloc 8 \arr rw ->
            -- Fill [0..7]
            let rw' = writeManyFrom rw arr 0 [0,1,2,3,4,5,6,7]
            -- Split at index 4: left=[0,1,2,3], right=[4,5,6,7]
            in case split rw' 4 arr of
                MkSlice st rwL rwR arrL arrR ->
                    -- Modify left half
                    let rwL' = unsafeWrite rwL arrL 0 100.0
                    -- Modify right half
                        rwR' = unsafeWrite rwR arrR 0 400.0
                    -- Recombine
                        rw'' = combine st rwL' rwR'
                    in  freeze rw'' arr
    printf "  After split+modify: %s\n" (show result)
    let ok = result == [100.0, 1.0, 2.0, 3.0, 400.0, 5.0, 6.0, 7.0]
    printf "  Expected [100,1,2,3,400,5,6,7]: %s\n"
        (if ok then "OK" else "MISMATCH" :: String)

-- | Test 3: OpenMP DGEMM via withPtr (zero-copy C FFI)
test3_dgemm :: Int -> IO ()
test3_dgemm n = do
    printf "--- Test 3: OpenMP DGEMM via withPtr (N=%d) ---\n" n
    let nn = n * n
        (arrA, rwA0) = unsafeAlloc nn
        (arrB, rwB0) = unsafeAlloc nn
        (arrC, _rwC) = unsafeAlloc nn
        (arrRef, rwRef0) = unsafeAlloc nn

        -- Fill A and B (seq to force evaluation)
        !_rwA = fillMatrix rwA0 arrA n n
            (\i j -> fromIntegral (i + 1) * 0.01 + fromIntegral j * 0.001)
        !_rwB = fillMatrix rwB0 arrB n n
            (\i j -> fromIntegral (j + 1) * 0.01 - fromIntegral i * 0.001)

    -- Warmup (use withPtrRO for all — we manage ownership manually here)
    withPtrRO arrA \pA ->
        withPtrRO arrB \pB ->
        withPtrRO arrC \pC ->
        c_parallel_dgemm pA pB pC (fromIntegral n)

    -- Timed run
    t0 <- nowMs
    withPtrRO arrA \pA ->
        withPtrRO arrB \pB ->
        withPtrRO arrC \pC ->
        c_parallel_dgemm pA pB pC (fromIntegral n)
    t1 <- nowMs
    let ompMs = t1 - t0

    -- Haskell reference DGEMM
    let !_rwRef = linearDgemm rwRef0 n 0 n arrA arrB arrRef

    -- Compare
    let maxd = maxDiffArrays arrC arrRef nn

    printf "  OpenMP DGEMM: %.1f ms\n" ompMs
    printf "  Max diff vs Haskell ref: %.2e %s\n" maxd
        (if maxd < 1e-6 then "OK" else "MISMATCH" :: String)

-- | Test 4: Type-safe row-partitioned DGEMM via split
test4_tiled :: Int -> IO ()
test4_tiled n = do
    printf "--- Test 4: Row-partitioned DGEMM via split (N=%d) ---\n" n
    let nn = n * n
        half = n `div` 2

        -- Allocate and fill
        (arrA, rwA0) = unsafeAlloc nn
        (arrB, rwB0) = unsafeAlloc nn
        (arrC, rwC0) = unsafeAlloc nn

        !_rwA = fillMatrix rwA0 arrA n n
            (\i j -> fromIntegral (i + 1) * 0.01 + fromIntegral j * 0.001)
        !_rwB = fillMatrix rwB0 arrB n n
            (\i j -> fromIntegral (j + 1) * 0.01 - fromIntegral i * 0.001)

    -- Split C into top and bottom halves (zero-copy)
    let (c00, cMid, cLast) = case split rwC0 (half * n) arrC of
            MkSlice st rwTop rwBot cTop cBot ->
              let
                -- Top half: rows [0..half-1] of C (half*n elements)
                rwTop' = linearDgemm rwTop n 0    half arrA arrB cTop
                -- Bottom half: rows [half..n-1] of C
                rwBot' = linearDgemm rwBot n half half arrA arrB cBot
                -- Recombine
                rwC' = combine st rwTop' rwBot'
                -- Read a few elements to verify
                (v00, rwC1)   = unsafeRead rwC' arrC 0
                (vMid, rwC2)  = unsafeRead rwC1 arrC (half * n)
                (vLast, _)    = unsafeRead rwC2 arrC (nn - 1)
              in (v00, vMid, vLast)

    -- Compute expected values via full DGEMM for verification
    let (arrE, rwE0) = unsafeAlloc nn
        rwE = linearDgemm rwE0 n 0 n arrA arrB arrE
        (e00, rwE1)   = unsafeRead rwE  arrE 0
        (eMid, rwE2)  = unsafeRead rwE1 arrE (half * n)
        (eLast, _)    = unsafeRead rwE2 arrE (nn - 1)

    let ok00   = abs (c00   - e00)   < 1e-10
        okMid  = abs (cMid  - eMid)  < 1e-10
        okLast = abs (cLast - eLast) < 1e-10
    printf "  C[0,0]:     %.6f (expected %.6f) %s\n"
        c00 e00 (if ok00 then "OK" else "MISMATCH" :: String)
    printf "  C[%d,0]:   %.6f (expected %.6f) %s\n"
        half cMid eMid (if okMid then "OK" else "MISMATCH" :: String)
    printf "  C[%d,%d]: %.6f (expected %.6f) %s\n"
        (n-1) (n-1) cLast eLast (if okLast then "OK" else "MISMATCH" :: String)
    putStrLn "  Split/combine verified: both halves computed independently,"
    putStrLn "  then recombined — type system ensured disjoint access."

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Write multiple values starting at an index.
writeManyFrom :: RW s %1 -> DArray s -> Int -> [Double] -> RW s
writeManyFrom rw _ _ [] = rw
writeManyFrom rw arr i (x:xs) =
    writeManyFrom (unsafeWrite rw arr i x) arr (i + 1) xs

-- | Max element-wise diff between two arrays. Read-only.
maxDiffArrays :: DArray sa -> DArray sb -> Int -> Double
maxDiffArrays a b nn = go 0 0.0
  where
    go i !mx
        | i >= nn = mx
        | otherwise =
            let (va, _) = readRO a i
                (vb, _) = readRO b i
                d = abs (va - vb)
            in  go (i + 1) (max mx d)

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Phase 17: Linear Typed Arrays ==="
    putStrLn ""

    nthreads <- c_get_omp_num_threads
    printf "OpenMP threads: %d\n" (fromIntegral nthreads :: Int)
    putStrLn ""

    test1_basic
    putStrLn ""

    test2_split
    putStrLn ""

    test3_dgemm 128
    putStrLn ""

    test4_tiled 128
    putStrLn ""

    test4_tiled 256
    putStrLn ""

    putStrLn "--- Summary ---"
    putStrLn "  Linear tokens (RW s) enforce exclusive access at compile time."
    putStrLn "  split/combine partition arrays into disjoint regions — zero-copy."
    putStrLn "  withPtr passes pinned ByteArray# directly to C — zero marshalling."
    putStrLn "  No external dependencies (self-contained, ~150 lines)."
    putStrLn ""
    putStrLn "=== Done ==="
