{- |
   Phase 7: Real-world numerical workload — Dense matrix multiply.

   Calls an OpenMP-parallelized DGEMM (C = A*B) from Haskell via FFI,
   compares with a sequential Haskell implementation using direct pointer
   access (no list conversion). Validates that the RTS-backed OpenMP
   runtime handles sustained compute workloads with large memory footprints.

   Run: ./matmul_demo +RTS -N4
-}
module Main where

import Control.Monad (forM_)
import Foreign
import Foreign.C
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf

-- FFI imports
foreign import ccall safe "parallel_dgemm"
    c_parallel_dgemm :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO ()

foreign import ccall safe "get_omp_num_threads"
    c_get_omp_num_threads :: IO CInt

-- | Wall clock time in milliseconds
nowMs :: IO Double
nowMs = do
    ns <- getMonotonicTimeNSec
    return (fromIntegral ns / 1e6)

-- | Sequential matrix multiply using direct pointer access (row-major, NxN)
haskellDgemm :: Int -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
haskellDgemm n pA pB pC =
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

-- | Fill matrix with deterministic values
fillMatrix :: Int -> Ptr CDouble -> (Int -> Int -> Double) -> IO ()
fillMatrix n p f =
    forM_ [0..n-1] $ \i ->
        forM_ [0..n-1] $ \j ->
            pokeElemOff p (i * n + j) (realToFrac (f i j))

-- | Compute max element-wise difference between two matrices
maxDiff :: Int -> Ptr CDouble -> Ptr CDouble -> IO Double
maxDiff n p1 p2 = do
    let go !mx idx
            | idx >= n * n = return mx
            | otherwise = do
                a <- peekElemOff p1 idx
                b <- peekElemOff p2 idx
                let d = abs (realToFrac a - realToFrac b :: Double)
                go (max mx d) (idx + 1)
    go 0.0 0

main :: IO ()
main = do
    putStrLn "=== Phase 7: Dense Matrix Multiply (DGEMM) ==="
    putStrLn ""

    nthreads <- c_get_omp_num_threads
    printf "OpenMP threads: %d\n" (fromIntegral nthreads :: Int)
    putStrLn ""

    -- Test multiple sizes
    let sizes = [128, 256, 512, 1024]

    -- Header
    printf "%-6s  %10s  %10s  %7s  %s\n"
        ("N" :: String) ("Haskell ms" :: String) ("OpenMP ms" :: String)
        ("Speedup" :: String) ("Correct" :: String)

    results <- mapM runSize sizes

    putStrLn ""
    putStrLn "--- Summary ---"
    mapM_ (\(n, hsMs, ompMs, spd, ok) ->
            printf "  N=%4d: Haskell %8.1f ms, OpenMP %8.1f ms, %.1fx speedup %s\n"
                n hsMs ompMs spd (if ok then "(OK)" else "(MISMATCH!)" :: String)
        ) results

    putStrLn ""
    putStrLn "=== Done ==="

runSize :: Int -> IO (Int, Double, Double, Double, Bool)
runSize n = do
    let nn = n * n
    allocaArray nn $ \pA ->
      allocaArray nn $ \pB ->
        allocaArray nn $ \pC_omp ->
          allocaArray nn $ \pC_hs -> do

            -- Fill A and B with deterministic values
            fillMatrix n pA (\i j -> fromIntegral (i + 1) * 0.01 + fromIntegral j * 0.001)
            fillMatrix n pB (\i j -> fromIntegral (j + 1) * 0.01 - fromIntegral i * 0.001)

            -- OpenMP DGEMM (warmup + timed)
            c_parallel_dgemm pA pB pC_omp (fromIntegral n)
            t0 <- nowMs
            c_parallel_dgemm pA pB pC_omp (fromIntegral n)
            t1 <- nowMs
            let ompMs = t1 - t0

            -- Haskell sequential DGEMM
            t2 <- nowMs
            haskellDgemm n pA pB pC_hs
            t3 <- nowMs
            let hsMs = t3 - t2

            -- Verify
            md <- maxDiff n pC_omp pC_hs
            let correct = md < 1e-6
                speedup = if ompMs > 0 then hsMs / ompMs else 0

            printf "%-6d  %10.1f  %10.1f  %6.1fx  %s\n"
                n hsMs ompMs speedup
                (if correct then "OK" else "MISMATCH" :: String)

            return (n, hsMs, ompMs, speedup, correct)
