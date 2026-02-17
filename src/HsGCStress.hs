{- |
   Phase 6: GC interaction test.

   Verifies that GHC's garbage collector does not disrupt OpenMP parallel
   regions. Our worker threads don't hold GHC Capabilities, so stop-the-world
   GC should not need to pause them.

   Test: run heavy Haskell allocation (triggering frequent GC) concurrently
   with repeated OpenMP parallel regions. Compare OpenMP latency with and
   without GC pressure.

   Run: ./gc_stress +RTS -N4 -s    (the -s flag shows GC stats)
-}
module Main where

import Control.Concurrent
import Control.Monad (forM_)
import Data.IORef
import Data.List (sort)
import Foreign
import Foreign.C
import GHC.Clock (getMonotonicTimeNSec)
import System.Mem (performGC)
import Text.Printf

-- FFI imports
foreign import ccall safe "repeated_parallel_sinsum"
    c_repeated_parallel_sinsum :: CInt -> CInt -> Ptr CDouble -> IO CDouble

foreign import ccall safe "get_omp_num_threads"
    c_get_omp_num_threads :: IO CInt

-- | Wall clock time in milliseconds
nowMs :: IO Double
nowMs = fromIntegral <$> getMonotonicTimeNSec >>= \ns -> return (ns / 1e6)

-- | Allocate aggressively to trigger GC.
--   Creates many short-lived lists to fill the nursery.
gcPressure :: Int -> IORef Int -> IO ()
gcPressure rounds counterRef = do
    forM_ [1..rounds] $ \_ -> do
        let !_ = sum [1..10000 :: Int]
        modifyIORef' counterRef (+1)

-- | Analyze latency array: returns (mean, p50, p99, max) in microseconds
analyzeLatencies :: Int -> Ptr CDouble -> IO (Double, Double, Double, Double)
analyzeLatencies n ptr = do
    vals <- peekArray n ptr
    let ds = sort (map realToFrac vals :: [Double])
        mean = sum ds / fromIntegral n
        p50 = ds !! (n `div` 2)
        p99 = ds !! (n * 99 `div` 100)
        mx = last ds
    return (mean, p50, p99, mx)

main :: IO ()
main = do
    putStrLn "=== Phase 6: GC Interaction Test ==="
    putStrLn ""

    nthreads <- c_get_omp_num_threads
    printf "OpenMP threads: %d\n" (fromIntegral nthreads :: Int)
    putStrLn ""

    let ompIters = 500     -- number of parallel regions
        ompWork  = 100000  -- iterations per region

    -- Warmup
    allocaArray ompIters $ \ptr -> do
        _ <- c_repeated_parallel_sinsum (fromIntegral ompWork) (fromIntegral ompIters) ptr
        return ()

    -- Test 1: OpenMP alone (baseline)
    putStrLn "--- Test 1: OpenMP alone (baseline) ---"
    t0 <- nowMs
    (mean1, p50_1, p99_1, max1) <- allocaArray ompIters $ \ptr -> do
        _ <- c_repeated_parallel_sinsum (fromIntegral ompWork) (fromIntegral ompIters) ptr
        analyzeLatencies ompIters ptr
    t1 <- nowMs
    printf "  %d regions in %.0f ms\n" ompIters (t1 - t0)
    printf "  Latency (us): mean=%.0f  p50=%.0f  p99=%.0f  max=%.0f\n" mean1 p50_1 p99_1 max1
    putStrLn ""

    -- Test 2: OpenMP + concurrent Haskell GC pressure (allocation-heavy)
    putStrLn "--- Test 2: OpenMP + allocation pressure ---"
    gcCounter <- newIORef (0 :: Int)
    gcDone <- newEmptyMVar
    ompDone <- newEmptyMVar

    _ <- forkIO $ do
        gcPressure 50000 gcCounter
        putMVar gcDone ()

    t2 <- nowMs
    _ <- forkIO $ do
        result <- allocaArray ompIters $ \ptr -> do
            _ <- c_repeated_parallel_sinsum (fromIntegral ompWork) (fromIntegral ompIters) ptr
            analyzeLatencies ompIters ptr
        putMVar ompDone result

    (mean2, p50_2, p99_2, max2) <- takeMVar ompDone
    takeMVar gcDone
    t3 <- nowMs
    gcCount <- readIORef gcCounter
    printf "  %d regions + %d alloc rounds in %.0f ms\n" ompIters gcCount (t3 - t2)
    printf "  Latency (us): mean=%.0f  p50=%.0f  p99=%.0f  max=%.0f\n" mean2 p50_2 p99_2 max2
    putStrLn ""

    -- Test 3: OpenMP + forced major GCs
    putStrLn "--- Test 3: OpenMP + forced major GC ---"
    gcDone2 <- newEmptyMVar
    ompDone2 <- newEmptyMVar

    _ <- forkIO $ do
        forM_ [1..20 :: Int] $ \_ -> do
            performGC
            threadDelay 5000  -- 5ms between GCs
        putMVar gcDone2 ()

    t4 <- nowMs
    _ <- forkIO $ do
        result <- allocaArray ompIters $ \ptr -> do
            _ <- c_repeated_parallel_sinsum (fromIntegral ompWork) (fromIntegral ompIters) ptr
            analyzeLatencies ompIters ptr
        putMVar ompDone2 result

    (mean3, p50_3, p99_3, max3) <- takeMVar ompDone2
    takeMVar gcDone2
    t5 <- nowMs
    printf "  %d regions + 20 major GCs in %.0f ms\n" ompIters (t5 - t4)
    printf "  Latency (us): mean=%.0f  p50=%.0f  p99=%.0f  max=%.0f\n" mean3 p50_3 p99_3 max3
    putStrLn ""

    -- Summary
    putStrLn "--- Summary ---"
    printf "  Baseline p99: %.0f us,  max: %.0f us\n" p99_1 max1
    printf "  + alloc  p99: %.0f us,  max: %.0f us\n" p99_2 max2
    printf "  + GC     p99: %.0f us,  max: %.0f us\n" p99_3 max3
    let worstP99 = max p99_2 p99_3
        worstMax = max max2 max3
    printf "  Worst p99 ratio: %.2fx\n" (worstP99 / p99_1)
    printf "  Worst max ratio: %.2fx\n" (worstMax / max1)
    putStrLn ""

    if worstMax / max1 < 5.0
        then putStrLn "GC has minimal impact on OpenMP tail latency."
        else printf "GC causes significant tail latency increase (%.1fx).\n" (worstMax / max1)
    putStrLn ""
    putStrLn "=== Done ==="
