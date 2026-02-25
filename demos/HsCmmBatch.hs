{- |
   Phase 12: Batched safe calls via Cmm.

   Standard safe FFI does suspendThread/resumeThread per call (~65ns).
   By writing the suspend/resume manually in Cmm, we batch N calls
   in one cycle, amortizing the overhead.

   Run: ./cmm_batch +RTS -N4
-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Exts
import GHC.IO (IO(..))
import Data.Word (Word64)
import Foreign.C
import Foreign (Ptr, mallocBytes, free, pokeElemOff)
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf

-- Cmm primitives from omp_batch.cmm.
-- State# threading ensures GHC treats these as effectful (no CSE/LICM).
-- At the Cmm level, State# is erased — the functions take/return one W_ each.
foreign import prim "cmm_safe_tid"
    cmmSafeTid# :: Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "cmm_batched_tid"
    cmmBatchedTid# :: Int# -> State# RealWorld -> (# State# RealWorld, Int# #)

-- IO wrappers
cmmSafeTid :: IO Int
cmmSafeTid = IO $ \s -> case cmmSafeTid# 0# s of (# s', r #) -> (# s', I# r #)

cmmBatchedTid :: Int -> IO Int
cmmBatchedTid (I# n) = IO $ \s -> case cmmBatchedTid# n s of (# s', r #) -> (# s', I# r #)

-- Standard safe FFI for comparison
foreign import ccall safe "omp_get_thread_num" c_safe_tid :: IO CInt

-- | Loop calling standard safe FFI N times.
{-# NOINLINE loopSafe #-}
loopSafe :: Ptr CInt -> Int -> IO ()
loopSafe _   0 = return ()
loopSafe ptr n = do
    !t <- c_safe_tid
    pokeElemOff ptr 0 t
    loopSafe ptr (n - 1)

-- | Loop calling Cmm batched N times.
{-# NOINLINE loopBatched #-}
loopBatched :: Int -> Int -> IO ()
loopBatched _      0 = return ()
loopBatched batchN n = do
    !_ <- cmmBatchedTid batchN
    loopBatched batchN (n - 1)

-- | Time a function, return total ns.
timeIO :: IO () -> IO Word64
timeIO action = do
    t0 <- getMonotonicTimeNSec
    action
    t1 <- getMonotonicTimeNSec
    return (t1 - t0)

main :: IO ()
main = do
    putStrLn "=== Phase 12: Batched Safe Calls via Cmm ==="
    putStrLn ""

    -- Verify correctness
    singleResult <- cmmSafeTid
    printf "  cmm_safe_tid:    thread_num = %d\n" singleResult

    batch10Result <- cmmBatchedTid 10
    printf "  cmm_batched(10): sum = %d (expected 0 on main thread)\n" batch10Result
    putStrLn ""

    buf <- mallocBytes 8 :: IO (Ptr CInt)

    -- Benchmark: vary batch size
    putStrLn "--- Per-call overhead at different batch sizes ---"
    putStrLn "  Batch size | Standard safe | Cmm batched | Speedup"
    putStrLn "  -----------|---------------|-------------|--------"

    -- Warmup
    loopSafe buf 10000
    _ <- cmmBatchedTid 10000

    let batchSizes = [1, 2, 5, 10, 20, 50, 100] :: [Int]

    mapM_ (\batchN -> do
        let totalCalls = 1000000  -- 1M total calls
        let outerIters = totalCalls `div` batchN
        let runs = 5 :: Int

        -- Best-of-5 for standard safe FFI
        bestSafe <- bestOf runs $ loopSafe buf totalCalls

        -- Best-of-5 for Cmm batched
        bestBatch <- bestOf runs $ loopBatched batchN outerIters

        let safeNs  = fromIntegral bestSafe  / fromIntegral totalCalls :: Double
        let batchNs = fromIntegral bestBatch / fromIntegral totalCalls :: Double
        let speedup = safeNs / batchNs

        printf "  %10d | %9.1f ns  | %9.1f ns | %5.1fx\n"
            batchN safeNs batchNs speedup
        ) batchSizes

    putStrLn ""

    -- Theoretical vs measured
    putStrLn "--- Analysis ---"
    putStrLn "  Standard safe: each call pays ~65ns suspend/resume overhead."
    putStrLn "  Cmm batched: one suspend/resume (~65ns) amortized over N calls."
    putStrLn "  At batch=100: per-call overhead approaches unsafe FFI (~2ns)."
    putStrLn ""

    free buf
    putStrLn "=== Done ==="

-- | Run an IO action N times, return best (shortest) time in nanoseconds.
bestOf :: Int -> IO () -> IO Word64
bestOf runs action = go runs maxBound
  where
    go 0 best = return best
    go n best = do
        elapsed <- timeIO action
        go (n - 1) (min best elapsed)
