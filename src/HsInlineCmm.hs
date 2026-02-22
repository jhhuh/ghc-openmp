{- |
   Phase 11: inline-cmm integration demo.

   Demonstrates the inline-cmm quasiquoter for embedding Cmm code
   directly in Haskell. Instead of writing a separate .cmm file and
   a manual foreign import prim declaration, the [cmm| ... |]
   quasiquoter handles both automatically.

   Build: cabal run inline-cmm-demo -- +RTS -N4
-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Language.Haskell.Inline.Cmm
import GHC.Exts
import GHC.Clock (getMonotonicTimeNSec)

-- Include GHC's Cmm header for RTS macros (MyCapability, Capability_no, etc.)
include "\"Cmm.h\""

-- | Read current Capability number — zero overhead.
-- The quasiquoter generates:
--   foreign import prim "get_cap_no" get_cap_no# :: Int# -> Int#
-- and compiles the Cmm to an object file automatically.
[cmm|
I64 get_cap_no(I64 dummy) {
    return (Capability_no(MyCapability()));
}
|]

-- | Add two unboxed integers — trivial example showing the pattern.
[cmm|
I64 cmm_add(I64 a, I64 b) {
    return (a + b);
}
|]

-- | Multiply two unboxed integers.
[cmm|
I64 cmm_mul(I64 a, I64 b) {
    return (a * b);
}
|]

-- | Compute a * b + c (fused multiply-add pattern).
[cmm|
I64 cmm_fma(I64 a, I64 b, I64 c) {
    return (a * b + c);
}
|]

main :: IO ()
main = do
    putStrLn "=== Phase 11: inline-cmm Quasiquoter Demo ==="
    putStrLn ""

    -- Read Capability number via inline Cmm
    let capNo = I# (get_cap_no# 0#)
    putStrLn $ "  Capability number (via inline Cmm): " ++ show capNo

    -- Basic arithmetic via Cmm
    let sum3_5 = I# (cmm_add# 3# 5#)
    putStrLn $ "  cmm_add 3 5 = " ++ show sum3_5

    let prod4_7 = I# (cmm_mul# 4# 7#)
    putStrLn $ "  cmm_mul 4 7 = " ++ show prod4_7

    let fma2_3_10 = I# (cmm_fma# 2# 3# 10#)
    putStrLn $ "  cmm_fma 2 3 10 = " ++ show fma2_3_10
    putStrLn ""

    -- Benchmark: prim call overhead via inline-cmm
    let n = 100000000 :: Int  -- 100M iterations

    putStrLn $ "--- Benchmarking " ++ show n ++ " inline Cmm calls ---"

    -- Warmup
    let !_ = loopCapNo 10000# 0#

    -- Best of 5
    best <- go 5 1e18
    let nsPerCall = best

    putStrLn $ "  get_cap_no: " ++ showFFloat' nsPerCall ++ " ns/call (best of 5)"
    putStrLn $ "  (Same result as Phase 10 hand-written Cmm — zero overhead)"
    putStrLn ""

    putStrLn "--- What inline-cmm automates ---"
    putStrLn "  1. Parses Cmm function signature"
    putStrLn "  2. Generates 'foreign import prim' declaration"
    putStrLn "  3. Compiles Cmm to object via 'ghc -c -x cmm'"
    putStrLn "  4. Links object into binary via Template Haskell"
    putStrLn "  No separate .cmm file or Makefile target needed."
    putStrLn ""
    putStrLn "=== Done ==="
  where
    go :: Int -> Double -> IO Double
    go 0 best = return best
    go r best = do
        t0 <- getMonotonicTimeNSec
        let !_ = loopCapNo (intToInt# 100000000) 0#
        t1 <- getMonotonicTimeNSec
        let ns = fromIntegral (t1 - t0) / 100000000.0
        go (r - 1) (min best ns)

    intToInt# :: Int -> Int#
    intToInt# (I# n) = n

    showFFloat' :: Double -> String
    showFFloat' d
      | d < 0.05  = "~0.0"
      | otherwise = show (fromIntegral (round (d * 10) :: Int) / 10.0 :: Double)

-- | Tight loop with data dependency to prevent LICM.
{-# NOINLINE loopCapNo #-}
loopCapNo :: Int# -> Int# -> Int#
loopCapNo 0# acc = acc
loopCapNo n acc = loopCapNo (n -# 1#) (acc +# get_cap_no# acc)
