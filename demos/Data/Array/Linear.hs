{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{- |
   Phase 17: Linear typed arrays for type-safe parallel FFI.

   Inspired by konn/linear-extra's borrowable SArray, but self-contained
   (~150 lines, no external deps) and integrated with Phase 16's unboxed
   primops (readDoubleArray#/writeDoubleArray# instead of Storable).

   Key idea: linearity is on the *tokens* (RW s), not the array itself.
   The array (DArray s) is passed as a regular argument. You need the
   right token to read/write, and split/combine tracks disjoint ownership
   at the type level.
-}
module Data.Array.Linear
    ( -- * Types
      DArray
    , RW(..)
    , SlicesTo
    , Slice(..)
    , Ur(..)
      -- * Allocation
    , alloc
    , allocFill
    , unsafeAlloc
      -- * Element access
    , unsafeRead
    , unsafeWrite
    , fill
    , size
      -- * Split / combine (zero-copy)
    , split
    , halve
    , combine
    , parCombine
      -- * C FFI
    , withPtr
    , unsafeWithPtr
      -- * Extraction
    , freeze
    , unsafeReadAll
    ) where

import GHC.Exts
import GHC.IO (IO(..))
import Foreign (Ptr(..), plusPtr)
import Foreign.C (CDouble(..))
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

------------------------------------------------------------------------
-- Core types
------------------------------------------------------------------------

-- | Unrestricted wrapper — lets a value escape a linear context.
data Ur a where
    Ur :: a -> Ur a

-- | Linear read-write token. Proves exclusive access to region @s@.
-- Zero-cost at runtime (single constructor, no fields).
data RW s where
    MkRW :: RW s

-- | Proof that region @s@ was split into regions @l@ and @r@.
-- Zero-cost witness — only exists to tie the types together.
data SlicesTo s l r where
    MkSlicesTo :: SlicesTo s l r

-- | Result of splitting an array.
data Slice s where
    MkSlice :: !(SlicesTo s l r)
            -> RW l %1
            -> RW r %1
            -> !(DArray l)
            -> !(DArray r)
            -> Slice s

-- | A mutable array of Doubles with a phantom region parameter @s@.
-- The array itself is NOT held linearly — linearity is enforced by
-- the 'RW' tokens. Uses a pinned MutableByteArray# for zero-copy
-- C FFI via 'mutableByteArrayContents#'.
--
-- The offset field enables zero-copy slicing: a slice is just a
-- different (offset, length) view into the same underlying buffer.
data DArray s = DArray
    { _daLen :: {-# UNPACK #-} !Int
    , _daBuf :: !(MutableByteArray# RealWorld)
    , _daOff :: {-# UNPACK #-} !Int
    }

------------------------------------------------------------------------
-- Allocation
------------------------------------------------------------------------

-- | Allocate a DArray of @n@ doubles. The array is pinned (not moved
-- by GC) so its address can be passed to C. CPS-style: the @forall s@
-- ensures the array and tokens can't escape the callback.
alloc :: Int -> (forall s. DArray s -> RW s %1 -> Ur b) %1 -> Ur b
alloc n f = case runAlloc n of (arr, rw) -> f arr rw
{-# INLINE alloc #-}

-- | Allocate and fill every element with a value.
allocFill :: Int -> Double -> (forall s. DArray s -> RW s %1 -> Ur b) %1 -> Ur b
allocFill n v f = alloc n (\arr rw -> let rw' = fill rw arr v in f arr rw')
{-# INLINE allocFill #-}

-- | Allocate without CPS. The caller is responsible for using the
-- token linearly. Less safe than 'alloc' (no scoped @forall s@) but
-- more ergonomic for demos and tests.
unsafeAlloc :: Int -> (DArray s, RW s)
unsafeAlloc = runAlloc

-- Internal: actually allocate. Uses unsafeDupablePerformIO to ensure
-- the allocation is properly sequenced with subsequent reads/writes.
runAlloc :: Int -> (DArray s, RW s)
runAlloc (I# n) = case unsafeDupablePerformIO (IO $ \s0 ->
    case newPinnedByteArray# (n *# 8#) s0 of
        (# s1, mba #) -> (# s1, DArray (I# n) mba 0 #)) of
    arr -> (arr, MkRW)
{-# NOINLINE runAlloc #-}

------------------------------------------------------------------------
-- Element access
------------------------------------------------------------------------

-- | Read element at index @i@. No bounds checking. No CDouble boxing.
unsafeRead :: RW s %1 -> DArray s -> Int -> (Double, RW s)
unsafeRead MkRW (DArray _ mba (I# off)) (I# i) =
    case unsafeDupablePerformIO (IO $ \s ->
        case readDoubleArray# mba (off +# i) s of
            (# s', d #) -> (# s', D# d #)) of
    val -> (val, MkRW)
{-# NOINLINE unsafeRead #-}

-- | Write element at index @i@. No bounds checking. No CDouble boxing.
unsafeWrite :: RW s %1 -> DArray s -> Int -> Double -> RW s
unsafeWrite MkRW (DArray _ mba (I# off)) (I# i) (D# d) =
    case unsafeDupablePerformIO (IO $ \s ->
        case writeDoubleArray# mba (off +# i) d s of
            s' -> (# s', () #)) of
    () -> MkRW
{-# NOINLINE unsafeWrite #-}

-- | Fill all elements with a value.
fill :: RW s %1 -> DArray s -> Double -> RW s
fill MkRW (DArray (I# n) mba (I# off)) (D# d) =
    case unsafeDupablePerformIO (IO $ \s0 ->
        let go s i
                | isTrue# (i >=# n) = s
                | otherwise = case writeDoubleArray# mba (off +# i) d s of
                    s' -> go s' (i +# 1#)
        in (# go s0 0#, () #)) of
    () -> MkRW
{-# NOINLINE fill #-}

-- | Get the size of the array.
size :: DArray s -> Int
size (DArray n _ _) = n
{-# INLINE size #-}

------------------------------------------------------------------------
-- Split / combine (zero-copy)
------------------------------------------------------------------------

-- | Split an array at index @k@: left gets elements [0..k-1],
-- right gets elements [k..n-1]. Zero-copy — both halves share
-- the same underlying buffer, just with different offsets.
--
-- The original 'RW s' is consumed; you get back 'RW l' and 'RW r'
-- for the two halves. The 'SlicesTo s l r' witness lets you
-- recombine them later via 'combine'.
split :: RW s %1 -> Int -> DArray s -> Slice s
split MkRW k (DArray n mba off) =
    MkSlice MkSlicesTo MkRW MkRW
        (DArray k       mba off)
        (DArray (n - k) mba (off + k))
{-# NOINLINE split #-}

-- | Split an array in half.
halve :: RW s %1 -> DArray s -> Slice s
halve rw arr = split rw (size arr `div` 2) arr
{-# NOINLINE halve #-}

-- | Recombine two halves. Consumes both child tokens and the
-- 'SlicesTo' witness, returning the parent token.
-- Zero-cost: no allocation, no copying.
combine :: SlicesTo s l r %1 -> RW l %1 -> RW r %1 -> RW s
combine MkSlicesTo MkRW MkRW = MkRW
{-# NOINLINE combine #-}

-- | Like 'combine' but evaluates both child tokens in parallel
-- using GHC sparks. The left token is sparked for parallel
-- evaluation; the right token is evaluated on the current thread.
-- Then waits for the left to complete before returning the
-- parent token.
--
-- Uses 'unsafePerformIO' (which includes 'noDuplicate#') to
-- prevent thunk duplication — essential when the tokens are
-- produced by destructive array operations.
--
-- Usage:
--
-- @
-- case halve rw arrOut of
--     MkSlice st rwL rwR arrL arrR ->
--         let rwL' = transform rwL arrIn arrL
--             rwR' = transform rwR arrIn arrR
--         in parCombine st rwL' rwR'
-- @
parCombine :: SlicesTo s l r %1 -> RW l %1 -> RW r %1 -> RW s
parCombine = unsafeCoerce# parCombineGo
  where
    parCombineGo :: SlicesTo s l r -> RW l -> RW r -> RW s
    parCombineGo MkSlicesTo l r =
        case unsafePerformIO (IO $ \s0 ->
            case spark# l s0 of { (# s1, _ #) ->
            case seq# r s1 of { (# s2, _ #) ->
            case seq# l s2 of { (# s3, _ #) ->
            (# s3, () #) }}}) of
        () -> MkRW
    {-# NOINLINE parCombineGo #-}
{-# NOINLINE parCombine #-}

------------------------------------------------------------------------
-- C FFI
------------------------------------------------------------------------

-- | Get a raw 'Ptr CDouble' to the array's data for passing to C.
-- The callback receives the pointer; the array is kept alive via
-- 'touch#' after the callback returns. The 'RW' token is threaded
-- through, proving you have exclusive access during the call.
withPtr :: RW s %1 -> DArray s -> (Ptr CDouble -> IO a) -> (a, RW s)
withPtr MkRW (DArray _ mba (I# off)) action =
    let result = case runRW# (\s0 ->
            let addr = mutableByteArrayContents# mba
                ptr  = Ptr addr `plusPtr` (I# (off *# 8#))
            in case unIO (action ptr) s0 of
                (# s1, r #) ->
                    case touch# mba s1 of
                        _ -> r) of
            r -> r
    in (result, MkRW)
{-# INLINE withPtr #-}

-- | Like 'withPtr' but in IO and without requiring a linear token.
-- Safe for read-only access or when ownership is tracked manually.
unsafeWithPtr :: DArray s -> (Ptr CDouble -> IO a) -> IO a
unsafeWithPtr (DArray _ mba (I# off)) action = IO $ \s0 ->
    let addr = mutableByteArrayContents# mba
        ptr  = Ptr addr `plusPtr` (I# (off *# 8#))
    in case unIO (action ptr) s0 of
        (# s1, r #) -> case touch# mba s1 of
            s2 -> (# s2, r #)

-- Unwrap IO for use with runRW#
unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f
{-# INLINE unIO #-}

------------------------------------------------------------------------
-- Extraction
------------------------------------------------------------------------

-- | Read all elements into a list. Consumes the token.
freeze :: RW s %1 -> DArray s -> Ur [Double]
freeze MkRW (DArray (I# n) mba (I# off)) =
    case unsafeDupablePerformIO (IO $ \s0 ->
        let go s i acc
                | isTrue# (i <# 0#) = (# s, acc #)
                | otherwise =
                    case readDoubleArray# mba (off +# i) s of
                        (# s', d #) -> go s' (i -# 1#) (D# d : acc)
        in go s0 (n -# 1#) []) of
    xs -> Ur xs
{-# NOINLINE freeze #-}

-- | Read all elements without consuming the token (for verification).
unsafeReadAll :: RW s %1 -> DArray s -> ([Double], RW s)
unsafeReadAll MkRW (DArray (I# n) mba (I# off)) =
    case unsafeDupablePerformIO (IO $ \s0 ->
        let go s i acc
                | isTrue# (i <# 0#) = (# s, acc #)
                | otherwise =
                    case readDoubleArray# mba (off +# i) s of
                        (# s', d #) -> go s' (i -# 1#) (D# d : acc)
        in go s0 (n -# 1#) []) of
    xs -> (xs, MkRW)
{-# NOINLINE unsafeReadAll #-}
