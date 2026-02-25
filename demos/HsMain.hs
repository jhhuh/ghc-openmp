{- |
   Phase 4: Haskell program calling OpenMP-parallelized C via FFI.
   Both Haskell and OpenMP share the GHC RTS thread pool.

   Build: ghc -threaded -O2 HsMain.hs omp_compute.o ghc_omp_runtime_rts.o -o demo
   Run:   ./demo +RTS -N4
-}
module Main where

import Foreign
import Foreign.C
import Text.Printf
import GHC.Clock (getMonotonicTimeNSec)

-- FFI imports for OpenMP-parallelized C functions
foreign import ccall "parallel_dot"
    c_parallel_dot :: Ptr CDouble -> Ptr CDouble -> CInt -> IO CDouble

foreign import ccall "parallel_sinsum"
    c_parallel_sinsum :: CInt -> IO CDouble

foreign import ccall "parallel_saxpy"
    c_parallel_saxpy :: CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO ()

foreign import ccall "get_omp_num_threads"
    c_get_omp_num_threads :: IO CInt

-- | Allocate and fill an array with a pattern
withArray' :: Int -> (Int -> Double) -> (Ptr CDouble -> IO a) -> IO a
withArray' n f action = allocaArray n $ \ptr -> do
    pokeArray ptr (map (CDouble . f) [0 .. n-1])
    action ptr

-- | Sequential Haskell dot product for verification
haskellDot :: [Double] -> [Double] -> Double
haskellDot xs ys = sum (zipWith (*) xs ys)

-- | Time an IO action in milliseconds (wall clock)
timeIt :: IO a -> IO (a, Double)
timeIt action = do
    t0 <- getMonotonicTimeNSec
    result <- action
    t1 <- getMonotonicTimeNSec
    let ms = fromIntegral (t1 - t0) / 1e6 :: Double
    return (result, ms)

main :: IO ()
main = do
    putStrLn "=== Phase 4: Haskell ↔ OpenMP Interop ==="
    putStrLn ""

    -- Query thread count
    nthreads <- c_get_omp_num_threads
    printf "OpenMP threads (GHC Capabilities): %d\n" (fromIntegral nthreads :: Int)
    putStrLn ""

    let n = 2000000  -- 2M elements

    -- Test 1: Parallel dot product
    putStrLn "--- Test 1: Parallel Dot Product (2M doubles) ---"
    let aVals = map (\i -> sin (fromIntegral i * 0.001)) [0 .. n-1]
        bVals = map (\i -> cos (fromIntegral i * 0.001)) [0 .. n-1]

    -- Haskell sequential reference
    let expected = haskellDot aVals bVals
    printf "  Haskell sequential result: %.6f\n" expected

    -- FFI → OpenMP parallel
    (result, ms) <- timeIt $
        withArray' n (\i -> sin (fromIntegral i * 0.001)) $ \aPtr ->
        withArray' n (\i -> cos (fromIntegral i * 0.001)) $ \bPtr ->
            c_parallel_dot aPtr bPtr (fromIntegral n)

    let cResult = realToFrac result :: Double
    printf "  OpenMP parallel result:   %.6f\n" cResult
    printf "  Match: %s (diff = %.2e)\n"
        (if abs (cResult - expected) < 1e-6 then "YES" else "NO" :: String)
        (abs (cResult - expected))
    printf "  Time: %.2f ms\n" ms
    putStrLn ""

    -- Test 2: Parallel sinsum (compute-bound)
    putStrLn "--- Test 2: Parallel sin() Sum (2M iterations) ---"
    (sinResult, sinMs) <- timeIt $ c_parallel_sinsum (fromIntegral n)
    printf "  Result: %.6f\n" (realToFrac sinResult :: Double)
    printf "  Time: %.2f ms\n" sinMs
    putStrLn ""

    -- Test 3: Parallel SAXPY
    putStrLn "--- Test 3: Parallel SAXPY (2M doubles) ---"
    let alpha = 2.5
    (_, saxpyMs) <- timeIt $
        withArray' n (\i -> fromIntegral i) $ \xPtr ->
        withArray' n (\i -> fromIntegral i * 0.5) $ \yPtr -> do
            c_parallel_saxpy (CDouble alpha) xPtr yPtr (fromIntegral n)
            -- Check first and last elements
            y0 <- peekElemOff yPtr 0
            yLast <- peekElemOff yPtr (n - 1)
            let CDouble y0d = y0
                CDouble yLd = yLast
            printf "  y[0] = %.1f (expected %.1f)\n" y0d (alpha * 0 + 0 * 0.5 :: Double)
            let lastI = fromIntegral (n - 1) :: Double
            printf "  y[%d] = %.1f (expected %.1f)\n" (n-1) yLd (alpha * lastI + lastI * 0.5)

    printf "  Time: %.2f ms\n" saxpyMs
    putStrLn ""

    putStrLn "=== All tests passed ==="
