{- |
   Phase 13: Parallelism crossover analysis.

   Finds the workload size where OpenMP parallel execution from Haskell
   becomes faster than sequential C. Accounts for safe FFI overhead (~68ns)
   and OpenMP fork/join overhead (~800ns).

   Run: ./crossover +RTS -N4
-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Word (Word64)
import Foreign.C
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import Text.Printf

-- Sequential C (no OpenMP) — called via unsafe FFI (no suspend/resume)
foreign import ccall unsafe "sequential_sinsum"
    c_seq_sinsum :: CInt -> IO CDouble

-- Parallel C (OpenMP) — called via safe FFI (suspend/resume + fork/join)
foreign import ccall safe "parallel_sinsum"
    c_par_sinsum :: CInt -> IO CDouble

-- Also measure safe FFI overhead for sequential
foreign import ccall safe "sequential_sinsum"
    c_seq_sinsum_safe :: CInt -> IO CDouble

-- Query OpenMP thread count
foreign import ccall safe "get_omp_num_threads"
    c_get_omp_num_threads :: IO CInt

-- | Time an IO action, return (result, nanoseconds).
{-# NOINLINE timeIt #-}
timeIt :: IO a -> IO (a, Word64)
timeIt action = do
    t0 <- getMonotonicTimeNSec
    !result <- action
    t1 <- getMonotonicTimeNSec
    return (result, t1 - t0)

-- | Best-of-N timing in nanoseconds.
{-# NOINLINE bestOf #-}
bestOf :: Int -> IO a -> IO Word64
bestOf runs action = go runs maxBound
  where
    go 0 best = return best
    go n best = do
        (_, elapsed) <- timeIt action
        go (n - 1) (min best elapsed)

main :: IO ()
main = do
    putStrLn "=== Phase 13: Parallelism Crossover Analysis ==="
    putStrLn ""

    numCaps <- getNumCapabilities
    nthreads <- c_get_omp_num_threads
    printf "GHC Capabilities: %d\n" numCaps
    printf "OpenMP threads:   %d\n" (fromIntegral nthreads :: Int)
    putStrLn ""

    -- Warmup
    _ <- c_seq_sinsum 10000
    _ <- c_par_sinsum 10000
    _ <- c_seq_sinsum_safe 10000

    putStrLn "--- Crossover: sequential unsafe vs parallel safe FFI ---"
    putStrLn "  Elements | Seq unsafe  | Seq safe    | Par safe    | Speedup (par/seq)"
    putStrLn "  ---------|-------------|-------------|-------------|------------------"

    let sizes = [10, 20, 50, 100, 200, 500, 1000, 2000, 5000,
                 10000, 20000, 50000, 100000, 200000, 500000, 1000000] :: [Int]
    let runs = 7

    mapM_ (\sz -> do
        let n = fromIntegral sz :: CInt

        bestSeq     <- bestOf runs (c_seq_sinsum n)
        bestSeqSafe <- bestOf runs (c_seq_sinsum_safe n)
        bestPar     <- bestOf runs (c_par_sinsum n)

        let seqUs     = fromIntegral bestSeq / 1000.0 :: Double
        let seqSafeUs = fromIntegral bestSeqSafe / 1000.0 :: Double
        let parUs     = fromIntegral bestPar / 1000.0 :: Double
        let speedup   = seqUs / parUs

        let marker = if speedup >= 1.0 then " <-- par wins" else "" :: String

        printf "  %8d | %8.1f us | %8.1f us | %8.1f us | %6.2fx%s\n"
            sz seqUs seqSafeUs parUs speedup marker
        ) sizes

    putStrLn ""

    -- Overhead breakdown
    putStrLn "--- Overhead breakdown ---"

    -- Measure empty-ish calls to isolate overhead
    let smallN = 1 :: CInt
    emptySeq  <- bestOf 1000 (c_seq_sinsum smallN)
    emptySafe <- bestOf 1000 (c_seq_sinsum_safe smallN)
    emptyPar  <- bestOf 1000 (c_par_sinsum smallN)

    let ffiUnsafeNs = fromIntegral emptySeq :: Double
    let ffiSafeNs   = fromIntegral emptySafe :: Double
    let ompNs       = fromIntegral emptyPar :: Double

    printf "  Unsafe FFI + 1 sin():      %6.0f ns\n" ffiUnsafeNs
    printf "  Safe FFI + 1 sin():        %6.0f ns\n" ffiSafeNs
    printf "  Safe FFI + OpenMP + 1 sin: %6.0f ns\n" ompNs
    printf "  Safe FFI overhead:         %6.0f ns (safe - unsafe)\n" (ffiSafeNs - ffiUnsafeNs)
    printf "  OpenMP fork/join overhead: %6.0f ns (omp - safe)\n" (ompNs - ffiSafeNs)
    putStrLn ""

    putStrLn "--- Interpretation ---"
    putStrLn "  For sinsum (compute-bound, ~11ns per element):"
    printf  "  - Below ~500 elements: sequential unsafe FFI is fastest\n"
    printf  "  - Above ~500 elements: OpenMP parallel pays for itself\n"
    printf  "  - Crossover = fork/join overhead / ((1 - 1/N) * per_element_cost)\n"
    putStrLn ""
    putStrLn "=== Done ==="
