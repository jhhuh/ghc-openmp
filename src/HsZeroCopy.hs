{-# LANGUAGE MagicHash, UnboxedTuples #-}
{- |
   Phase 16: Zero-copy FFI with pinned ByteArray.

   Eliminates boxing overhead at the Haskell↔OpenMP boundary:
   - Pinned ByteArray# instead of mallocArray/pokeArray (zero-copy to C)
   - writeDoubleArray#/readDoubleArray# instead of peekElemOff/pokeElemOff
     (no CDouble boxing)
   - indexDoubleOffAddr# for unboxed inner loop (no realToFrac)

   Compares wall-clock time against Phase 7's allocaArray approach on DGEMM.

   Run: ./zerocopy_demo +RTS -N4
-}
module Main where

import GHC.Exts
import GHC.IO (IO(..))
import Foreign (Ptr(..))
import Foreign.C (CDouble(..), CInt(..))
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peekElemOff, pokeElemOff)
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf
import Control.Monad (forM_)

-- FFI imports (same C kernels as Phase 7)
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
-- Pinned ByteArray utilities (zero-copy, no boxing)
------------------------------------------------------------------------

-- | A pinned mutable byte array. Pinned = not moved by GC, so we can
-- hand its address to C.
data PinnedDoubles = PinnedDoubles (MutableByteArray# RealWorld) Int

-- | Allocate a pinned array of n doubles.
newPinnedDoubles :: Int -> IO PinnedDoubles
newPinnedDoubles (I# n) = IO $ \s0 ->
    case newPinnedByteArray# (n *# 8#) s0 of
        (# s1, mba #) -> (# s1, PinnedDoubles mba (I# n) #)

-- | Get the raw Ptr for passing to C. The array must be kept alive
-- (via touchPinned) after the C call returns.
ptrOfPinned :: PinnedDoubles -> Ptr CDouble
ptrOfPinned (PinnedDoubles mba _) =
    Ptr (mutableByteArrayContents# mba)

-- | Keep the ByteArray alive — prevents GC from collecting it while C
-- holds a pointer to it.
touchPinned :: PinnedDoubles -> IO ()
touchPinned (PinnedDoubles mba _) = IO $ \s ->
    case touch# mba s of s' -> (# s', () #)

-- | Write a Double at index i. No CDouble boxing.
writeDouble :: PinnedDoubles -> Int -> Double -> IO ()
writeDouble (PinnedDoubles mba _) (I# i) (D# d) = IO $ \s ->
    case writeDoubleArray# mba i d s of s' -> (# s', () #)

-- | Read a Double at index i. No CDouble boxing.
readDouble :: PinnedDoubles -> Int -> IO Double
readDouble (PinnedDoubles mba _) (I# i) = IO $ \s ->
    case readDoubleArray# mba i s of (# s', d #) -> (# s', D# d #)

------------------------------------------------------------------------
-- Phase 7 baseline: allocaArray + peekElemOff (boxed)
------------------------------------------------------------------------

-- | Fill matrix using pokeElemOff (boxes every element as CDouble)
fillMatrixBoxed :: Int -> Ptr CDouble -> (Int -> Int -> Double) -> IO ()
fillMatrixBoxed n p f =
    forM_ [0..n-1] $ \i ->
        forM_ [0..n-1] $ \j ->
            pokeElemOff p (i * n + j) (realToFrac (f i j))

-- | Sequential DGEMM using peekElemOff (boxed CDouble ↔ Double conversion)
dgemmBoxed :: Int -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
dgemmBoxed n pA pB pC =
    forM_ [0..n-1] $ \i ->
        forM_ [0..n-1] $ \j -> do
            let go !acc k
                    | k >= n    = return acc
                    | otherwise = do
                        a <- peekElemOff pA (i * n + k)
                        b <- peekElemOff pB (k * n + j)
                        go (acc + realToFrac a * realToFrac b) (k + 1)
            s <- go (0.0 :: Double) 0
            pokeElemOff pC (i * n + j) (realToFrac s)

------------------------------------------------------------------------
-- Phase 16: pinned ByteArray + unboxed primops
------------------------------------------------------------------------

-- | Fill matrix using writeDoubleArray# (no boxing, no list allocation)
fillMatrixUnboxed :: PinnedDoubles -> Int -> (Int -> Int -> Double) -> IO ()
fillMatrixUnboxed (PinnedDoubles mba _) (I# n) f = IO $ \s0 ->
    let goI s i
            | isTrue# (i >=# n) = s
            | otherwise = goI (goJ s i 0#) (i +# 1#)
        goJ s i j
            | isTrue# (j >=# n) = s
            | otherwise =
                let !(D# d) = f (I# i) (I# j)
                in  case writeDoubleArray# mba (i *# n +# j) d s of
                        s' -> goJ s' i (j +# 1#)
    in  (# goI s0 0#, () #)

-- | Sequential DGEMM using readDoubleArray# (fully unboxed inner loop)
dgemmUnboxed :: Int -> PinnedDoubles -> PinnedDoubles -> PinnedDoubles -> IO ()
dgemmUnboxed (I# n) (PinnedDoubles mbaA _) (PinnedDoubles mbaB _) (PinnedDoubles mbaC _) = IO $ \s0 ->
    let goI s i
            | isTrue# (i >=# n) = s
            | otherwise = goI (goJ s i 0#) (i +# 1#)
        goJ s i j
            | isTrue# (j >=# n) = s
            | otherwise =
                case goK s i j 0# 0.0## of
                    (# s', acc #) ->
                        case writeDoubleArray# mbaC (i *# n +# j) acc s' of
                            s'' -> goJ s'' i (j +# 1#)
        goK s i j k acc
            | isTrue# (k >=# n) = (# s, acc #)
            | otherwise =
                case readDoubleArray# mbaA (i *# n +# k) s of
                    (# s', a #) ->
                        case readDoubleArray# mbaB (k *# n +# j) s' of
                            (# s'', b #) ->
                                goK s'' i j (k +# 1#) (acc +## (a *## b))
    in  (# goI s0 0#, () #)

-- | Read result from C output matrix via Addr# (no boxing)
readResultUnboxed :: Ptr CDouble -> Int -> IO Double
readResultUnboxed (Ptr addr) (I# i) =
    return (D# (indexDoubleOffAddr# addr i))

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

-- | Best-of-N timing
bestOf :: Int -> IO a -> IO (a, Double)
bestOf runs action = do
    -- warmup
    r <- action
    -- timed runs
    times <- mapM (\_ -> do
        t0 <- nowMs
        _ <- action
        t1 <- nowMs
        return (t1 - t0)) [1..runs]
    return (r, minimum times)

-- | Fill-only benchmark: isolates allocation + element-write overhead
benchFill :: Int -> IO (Double, Double)
benchFill n = do
    let nn = n * n

    -- Boxed: allocaArray + pokeElemOff with CDouble boxing
    (_, fillB) <- bestOf 5 $
        allocaArray nn $ \p ->
            fillMatrixBoxed n p (\i j -> fromIntegral (i + 1) * 0.01 + fromIntegral j * 0.001)

    -- Unboxed: pinned ByteArray + writeDoubleArray# (no boxing)
    pd <- newPinnedDoubles nn
    (_, fillU) <- bestOf 5 $
        fillMatrixUnboxed pd n (\i j -> fromIntegral (i + 1) * 0.01 + fromIntegral j * 0.001)

    return (fillB, fillU)

-- | DGEMM benchmark: Haskell sequential, boxed vs unboxed inner loop
benchDgemm :: Int -> IO (Double, Double)
benchDgemm n = do
    let nn = n * n

    -- Prepare data (unboxed, shared for both)
    pdA <- newPinnedDoubles nn
    pdB <- newPinnedDoubles nn
    fillMatrixUnboxed pdA n (\i j -> fromIntegral (i + 1) * 0.01 + fromIntegral j * 0.001)
    fillMatrixUnboxed pdB n (\i j -> fromIntegral (j + 1) * 0.01 - fromIntegral i * 0.001)

    -- Boxed DGEMM: copy to Ptr, then use peekElemOff/realToFrac
    let pA = ptrOfPinned pdA
        pB = ptrOfPinned pdB
    allocaArray nn $ \pC_boxed -> do
        (_, hsB) <- bestOf 3 $ do
            dgemmBoxed n pA pB pC_boxed
            touchPinned pdA; touchPinned pdB

        -- Unboxed DGEMM: direct readDoubleArray#
        pdC_unboxed <- newPinnedDoubles nn
        (_, hsU) <- bestOf 3 $ dgemmUnboxed n pdA pdB pdC_unboxed

        -- Verify match
        let pC_u = ptrOfPinned pdC_unboxed
        md <- maxDiffBoxed n pC_boxed pC_u
        touchPinned pdC_unboxed
        when (md > 1e-6) $
            printf "  WARNING: dgemm mismatch = %.2e\n" md

        return (hsB, hsU)

-- | OpenMP DGEMM: verify same C kernel performance with both memory types
benchOmp :: Int -> IO (Double, Double)
benchOmp n = do
    let nn = n * n

    -- Boxed path
    allocaArray nn $ \pA ->
      allocaArray nn $ \pB ->
        allocaArray nn $ \pC -> do
            fillMatrixBoxed n pA (\i j -> fromIntegral (i + 1) * 0.01 + fromIntegral j * 0.001)
            fillMatrixBoxed n pB (\i j -> fromIntegral (j + 1) * 0.01 - fromIntegral i * 0.001)
            c_parallel_dgemm pA pB pC (fromIntegral n)  -- warmup
            (_, ompB) <- bestOf 5 $ c_parallel_dgemm pA pB pC (fromIntegral n)

            -- Unboxed path
            pdA <- newPinnedDoubles nn
            pdB <- newPinnedDoubles nn
            pdC <- newPinnedDoubles nn
            fillMatrixUnboxed pdA n (\i j -> fromIntegral (i + 1) * 0.01 + fromIntegral j * 0.001)
            fillMatrixUnboxed pdB n (\i j -> fromIntegral (j + 1) * 0.01 - fromIntegral i * 0.001)
            let pa = ptrOfPinned pdA
                pb = ptrOfPinned pdB
                pc = ptrOfPinned pdC
            c_parallel_dgemm pa pb pc (fromIntegral n)  -- warmup
            touchPinned pdA; touchPinned pdB; touchPinned pdC
            (_, ompU) <- bestOf 5 $ do
                c_parallel_dgemm pa pb pc (fromIntegral n)
                touchPinned pdA; touchPinned pdB; touchPinned pdC

            return (ompB, ompU)

-- | Max element-wise diff (boxed, for Phase 7 comparison)
maxDiffBoxed :: Int -> Ptr CDouble -> Ptr CDouble -> IO Double
maxDiffBoxed n p1 p2 = do
    let go !mx idx
            | idx >= n * n = return mx
            | otherwise = do
                a <- peekElemOff p1 idx
                b <- peekElemOff p2 idx
                let d = abs (realToFrac a - realToFrac b :: Double)
                go (max mx d) (idx + 1)
    go 0.0 0

-- | Max element-wise diff (unboxed)
maxDiffUnboxed :: Int -> PinnedDoubles -> PinnedDoubles -> IO Double
maxDiffUnboxed n pd1 pd2 = do
    let nn = n * n
        go !mx idx
            | idx >= nn = return mx
            | otherwise = do
                a <- readDouble pd1 idx
                b <- readDouble pd2 idx
                let d = abs (a - b)
                go (max mx d) (idx + 1)
    go 0.0 0

-- | Cross-verify: compare boxed OpenMP result against unboxed OpenMP result
crossVerify :: Int -> IO ()
crossVerify n = do
    let nn = n * n

    -- Boxed path
    allocaArray nn $ \pA ->
      allocaArray nn $ \pB ->
        allocaArray nn $ \pC_boxed -> do

            fillMatrixBoxed n pA (\i j -> fromIntegral (i + 1) * 0.01 + fromIntegral j * 0.001)
            fillMatrixBoxed n pB (\i j -> fromIntegral (j + 1) * 0.01 - fromIntegral i * 0.001)
            c_parallel_dgemm pA pB pC_boxed (fromIntegral n)

            -- Unboxed path
            pdA <- newPinnedDoubles nn
            pdB <- newPinnedDoubles nn
            pdC <- newPinnedDoubles nn
            fillMatrixUnboxed pdA n (\i j -> fromIntegral (i + 1) * 0.01 + fromIntegral j * 0.001)
            fillMatrixUnboxed pdB n (\i j -> fromIntegral (j + 1) * 0.01 - fromIntegral i * 0.001)
            c_parallel_dgemm (ptrOfPinned pdA) (ptrOfPinned pdB) (ptrOfPinned pdC) (fromIntegral n)
            touchPinned pdA; touchPinned pdB; touchPinned pdC

            -- Compare
            let check !mx idx
                    | idx >= nn = return mx
                    | otherwise = do
                        a <- peekElemOff pC_boxed idx
                        b <- readDouble pdC idx
                        let d = abs (realToFrac a - b :: Double)
                        check (max mx d) (idx + 1)
            md <- check 0.0 0
            printf "  Cross-verify N=%d: max diff = %.2e %s\n"
                n md (if md < 1e-12 then "OK" else "MISMATCH" :: String)

when :: Bool -> IO () -> IO ()
when True  act = act
when False _   = return ()

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Phase 16: Zero-Copy FFI with Pinned ByteArray ==="
    putStrLn ""

    nthreads <- c_get_omp_num_threads
    printf "OpenMP threads: %d\n" (fromIntegral nthreads :: Int)
    putStrLn ""

    -- Cross-verify correctness
    putStrLn "--- Correctness: cross-verify boxed vs unboxed ---"
    crossVerify 128
    crossVerify 256
    crossVerify 512
    putStrLn ""

    -- Benchmark 1: Fill only (isolates allocation + element-write overhead)
    putStrLn "--- Benchmark 1: Matrix fill (alloc + write N*N doubles) ---"
    printf "  %-6s  %8s  %8s  %8s\n"
        ("N" :: String) ("Boxed" :: String) ("Unboxed" :: String) ("Speedup" :: String)
    forM_ [256, 512, 1024, 2048] $ \n -> do
        (fillB, fillU) <- benchFill n
        printf "  %-6d  %7.2f ms %7.2f ms  %5.2fx\n"
            n fillB fillU (fillB / fillU)
    putStrLn ""

    -- Benchmark 2: Haskell sequential DGEMM
    putStrLn "--- Benchmark 2: Haskell sequential DGEMM ---"
    printf "  %-6s  %8s  %8s  %8s\n"
        ("N" :: String) ("Boxed" :: String) ("Unboxed" :: String) ("Speedup" :: String)
    forM_ [128, 256, 512] $ \n -> do
        (hsB, hsU) <- benchDgemm n
        printf "  %-6d  %7.1f ms %7.1f ms  %5.2fx\n"
            n hsB hsU (hsB / hsU)
    putStrLn ""

    -- Benchmark 3: OpenMP DGEMM (sanity check — same C kernel)
    putStrLn "--- Benchmark 3: OpenMP DGEMM (same C kernel, different memory) ---"
    printf "  %-6s  %10s  %10s  %8s\n"
        ("N" :: String) ("allocaArray" :: String) ("ByteArray#" :: String) ("Ratio" :: String)
    forM_ [256, 512] $ \n -> do
        (ompB, ompU) <- benchOmp n
        printf "  %-6d  %8.1f ms  %8.1f ms  %5.2fx\n"
            n ompB ompU (ompB / ompU)
    putStrLn ""

    putStrLn "--- Summary ---"
    putStrLn "  Boxed:   allocaArray + pokeElemOff (CDouble boxing per element)"
    putStrLn "  Unboxed: pinned ByteArray# + writeDoubleArray# (no boxing)"
    putStrLn "  Zero-copy: byteArrayContents# gives C a direct Addr# to pinned memory"
    putStrLn ""
    putStrLn "=== Done ==="
