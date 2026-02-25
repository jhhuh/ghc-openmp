{- |
   Phase 10: Cmm primitives — calling convention benchmark.

   Compares three ways to read the thread/capability number:
   1. foreign import prim (Cmm) — reads BaseReg directly, zero FFI overhead
   2. foreign import ccall unsafe — C call, saves/restores STG registers
   3. foreign import ccall safe — C call + Capability release/reacquire

   Run: ./cmm_demo +RTS -N4
-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Exts
import GHC.IO (IO(..))
import Foreign.C
import Foreign (Ptr, mallocBytes, free, pokeElemOff, peekElemOff)
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf

-- Cmm primitive: zero FFI overhead, reads cap->no from BaseReg
foreign import prim "omp_prim_cap_no" primCapNo# :: Int# -> Int#

-- C via unsafe FFI (our OpenMP runtime)
foreign import ccall unsafe "omp_get_thread_num" c_unsafe_tid :: IO CInt
foreign import ccall unsafe "omp_get_num_procs" c_unsafe_nprocs :: IO CInt

-- C via safe FFI (our OpenMP runtime)
foreign import ccall safe "omp_get_thread_num" c_safe_tid :: IO CInt

-- | Pure Int# loop with data dependency chain to prevent LICM.
-- Each call's input depends on the previous result.
{-# NOINLINE loopPrim #-}
loopPrim :: Int# -> Int# -> Int#
loopPrim 0# acc = acc
loopPrim n acc =
    let !r = primCapNo# acc  -- data dependency: acc varies per iteration
    in loopPrim (n -# 1#) (acc +# r)

-- | Write results to a Ptr to prevent boxing.
-- This measures unsafe FFI call overhead without IO/boxing noise.
{-# NOINLINE loopUnsafePtr #-}
loopUnsafePtr :: Ptr CInt -> Int -> IO ()
loopUnsafePtr _   0 = return ()
loopUnsafePtr ptr n = do
    !t <- c_unsafe_tid
    pokeElemOff ptr 0 t
    loopUnsafePtr ptr (n - 1)

{-# NOINLINE loopSafePtr #-}
loopSafePtr :: Ptr CInt -> Int -> IO ()
loopSafePtr _   0 = return ()
loopSafePtr ptr n = do
    !t <- c_safe_tid
    pokeElemOff ptr 0 t
    loopSafePtr ptr (n - 1)

main :: IO ()
main = do
    putStrLn "=== Phase 10: Cmm Primitives — Calling Convention Benchmark ==="
    putStrLn ""

    -- Show values from each method
    let capNo = I# (primCapNo# 42#)
    printf "  Cmm prim:   cap_no = %d\n" capNo

    unsafeTid <- c_unsafe_tid
    unsafeNp <- c_unsafe_nprocs
    printf "  Unsafe FFI: thread_num = %d, num_procs = %d\n"
        (fromIntegral unsafeTid :: Int) (fromIntegral unsafeNp :: Int)

    safeTid <- c_safe_tid
    printf "  Safe FFI:   thread_num = %d\n" (fromIntegral safeTid :: Int)
    putStrLn ""

    -- Allocate scratch buffer for pointer-based loops
    buf <- mallocBytes 8 :: IO (Ptr CInt)

    -- Benchmark parameters
    let nFast = 100000000  -- 100M for prim (very fast)
    let nFFI  = 10000000   -- 10M for unsafe FFI
    let nSafe =  1000000   -- 1M for safe FFI
    let runs  = 5 :: Int

    putStrLn "--- Calling convention overhead (best of 5, interleaved) ---"

    -- Warmup
    let !_ = loopPrim 10000# 0#
    loopUnsafePtr buf 10000
    loopSafePtr buf 1000

    -- Interleaved best-of-N
    let go :: Int -> Double -> Double -> Double -> IO (Double, Double, Double)
        go 0 bp bu bs = return (bp, bu, bs)
        go r bp bu bs = do
            -- Prim
            t0 <- getMonotonicTimeNSec
            let !_ = loopPrim (intToInt# nFast) 0#
            t1 <- getMonotonicTimeNSec
            let p = fromIntegral (t1 - t0) / fromIntegral nFast

            -- Unsafe FFI
            t2 <- getMonotonicTimeNSec
            loopUnsafePtr buf nFFI
            t3 <- getMonotonicTimeNSec
            let u = fromIntegral (t3 - t2) / fromIntegral nFFI

            -- Safe FFI
            t4 <- getMonotonicTimeNSec
            loopSafePtr buf nSafe
            t5 <- getMonotonicTimeNSec
            let s = fromIntegral (t5 - t4) / fromIntegral nSafe

            go (r - 1) (min bp p) (min bu u) (min bs s)

    (primNs, unsafeNs, safeNs) <- go runs 1e18 1e18 1e18

    free buf

    printf "  prim:   %6.1f ns/call  (%d iters)\n" primNs nFast
    printf "  unsafe: %6.1f ns/call  (%d iters)\n" unsafeNs nFFI
    printf "  safe:   %6.1f ns/call  (%d iters)\n" safeNs nSafe
    putStrLn ""

    putStrLn "--- Overhead ratios ---"
    printf "  unsafe/prim: %5.1fx\n" (unsafeNs / primNs)
    printf "  safe/prim:   %5.1fx\n" (safeNs / primNs)
    printf "  safe/unsafe: %5.1fx\n" (safeNs / unsafeNs)
    putStrLn ""

    putStrLn "--- Interpretation ---"
    printf "  prim:   ~%.1f ns  (Cmm: register read, no FFI boundary)\n" primNs
    printf "  unsafe: ~%.0f ns  (C call: STG register save/restore)\n" unsafeNs
    printf "  safe:   ~%.0f ns  (+ suspendThread/resumeThread)\n" safeNs
    putStrLn ""
    putStrLn "=== Done ==="

-- GHC 9.10 compat: convert boxed Int to Int#
intToInt# :: Int -> Int#
intToInt# (I# n) = n
