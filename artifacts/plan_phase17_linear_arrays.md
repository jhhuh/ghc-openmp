# Plan: Phase 17 — Linear Typed Arrays for Type-Safe Parallel FFI

## Goal
Build a self-contained linear array module (inspired by `konn/linear-extra`'s
borrowable `SArray`) that provides type-safe parallel sub-array access with
zero-copy splits, integrated with Phase 16's unboxed primops. Demonstrate on
DGEMM with tiled parallel sub-matrix access.

## Why Not Use linear-extra Directly?

| Issue | Detail |
|-------|--------|
| Not on Hackage | Must vendor as git submodule |
| Last updated Dec 2023 | Untested on GHC 9.10 |
| Dependency chain | `linear-base` + `linear-generics` + `linear-witness` |
| Uses `Storable` internally | `pokeElemOff`/`peekElemOff` — re-introduces boxing we eliminated in Phase 16 |
| Known `free` bug on subslices | Must always `combine` before `free` |

We extract the core pattern (~100 lines) and combine it with our Phase 16
unboxed primops. Zero external dependencies beyond `base` and `ghc-prim`.

## Design

### Core Types

```haskell
{-# LANGUAGE LinearTypes, GADTs, MagicHash, UnboxedTuples #-}

-- | Linear read-write token. Consuming this proves you have exclusive access.
data RW s where
  MkRW :: RW s

-- | Array of doubles with phantom region parameter s.
-- NOT held linearly — linearity is enforced by tokens.
-- Uses pinned ByteArray# (Phase 16) for zero-copy C FFI.
data DArray s = DArray
  { daLen  :: {-# UNPACK #-} !Int
  , daBuf  :: {-# UNPACK #-} !(MutableByteArray# RealWorld)
  , daOff  :: {-# UNPACK #-} !Int  -- element offset into buffer (for slices)
  }

-- | Proof that s was split into l and r. Zero-cost witness.
data SlicesTo s l r = MkSlicesTo
```

### Key Operations

```haskell
-- Allocation (returns array + token)
alloc :: Int -> (forall s. DArray s -> RW s %1 -> Ur b) %1 -> Ur b

-- Element access (token in, token out)
unsafeRead  :: RW s %1 -> DArray s -> Int -> (Double, RW s)
unsafeWrite :: RW s %1 -> DArray s -> Int -> Double -> RW s

-- Zero-copy split (advancePtr-equivalent via offset)
split   :: RW s %1 -> Int -> DArray s -> Slice s
combine :: SlicesTo s l r %1 -> RW l %1 -> RW r %1 -> RW s

-- C FFI: get raw Ptr for foreign calls
withPtr :: RW s %1 -> DArray s -> (Ptr CDouble -> IO a) -> (a, RW s)
```

### Split/Combine: Zero-Copy via Offset

Unlike `linear-extra` which uses `advancePtr` on a `Ptr`, we use an integer
offset into the shared `MutableByteArray#`:

```haskell
data Slice s where
  MkSlice :: SlicesTo s l r %1 -> RW l %1 -> RW r %1
          -> DArray l -> DArray r -> Slice s

split :: RW s %1 -> Int -> DArray s -> Slice s
split MkRW k (DArray n buf off) =
    MkSlice MkSlicesTo MkRW MkRW
        (DArray k       buf off)           -- left:  [off .. off+k)
        (DArray (n - k) buf (off + k))     -- right: [off+k .. off+n)
```

Both halves share the same `MutableByteArray#`. The `SlicesTo` witness
ensures they're recombined before the parent token can be reconstructed.
No copying, no allocation — just arithmetic on the offset field.

### Integration with Phase 16 Primops

Element access uses `readDoubleArray#`/`writeDoubleArray#` with the offset:

```haskell
unsafeRead :: RW s %1 -> DArray s -> Int -> (Double, RW s)
unsafeRead MkRW (DArray _ buf off) i = unsafePerformIO $ IO $ \s ->
    case readDoubleArray# buf (unI# (off + i)) s of
        (# s', d #) -> (# s', (D# d, MkRW) #)
```

C FFI uses `mutableByteArrayContents#` + offset:

```haskell
withPtr :: RW s %1 -> DArray s -> (Ptr CDouble -> IO a) -> (a, RW s)
withPtr MkRW (DArray len buf off) action = unsafePerformIO $ do
    let base = Ptr (mutableByteArrayContents# buf)
        ptr  = base `plusPtr` (off * 8)  -- offset in bytes
    result <- action ptr
    IO $ \s -> case touch# buf s of s' -> (# s', (result, MkRW) #)
```

## Demo: Tiled DGEMM with Linear Arrays

The demo shows type-safe row-partitioned parallel DGEMM:

```haskell
-- Each OpenMP thread gets a disjoint slice of the output matrix C.
-- The split/combine ensures at the type level that:
-- 1. No two threads write to the same rows
-- 2. All slices are recombined before reading results
-- 3. The original array can't be accessed while split

tiledDgemm :: Int -> DArray s -> DArray s -> DArray s -> RW s %1 -> RW s
tiledDgemm n arrA arrB arrC rw =
    let half = n `div` 2
    in case split rw half arrC of
        MkSlice st rwTop rwBot cTop cBot ->
            -- rwTop: exclusive access to C[0..half-1, :]
            -- rwBot: exclusive access to C[half..n-1, :]
            -- Could hand these to different OpenMP threads
            let rwTop' = dgemmRows 0    half n arrA arrB cTop rwTop
                rwBot' = dgemmRows half n    n arrA arrB cBot rwBot
            in  combine st rwTop' rwBot'
```

This is the Haskell-side type encoding of what OpenMP does at runtime with
`#pragma omp parallel for schedule(static)` — partitioning the output matrix
into disjoint row blocks.

## Files

### New files
- `src/Data/Array/Linear.hs` — The linear array module (~150 lines)
- `src/HsLinearDemo.hs` — Phase 17 demo: tiled DGEMM with linear arrays

### Modified files
- `Makefile` — add `build/linear_demo` and `demo-linear` targets
- `README.md` — add Phase 17 to project structure and demo targets
- `docs/index.md` — add Section 14 (renumber others)

## Implementation Steps

1. **Write `Data/Array/Linear.hs`** — the linear array module
   - `DArray s`, `RW s`, `SlicesTo s l r`, `Slice s`
   - `alloc`, `unsafeRead`, `unsafeWrite`, `fill`
   - `split`, `combine`
   - `withPtr` for C FFI
   - `freeze` to extract results as a regular list/vector
   - Verify: compiles with `LinearTypes` on GHC 9.10.3

2. **Write `HsLinearDemo.hs`** — the demo
   - Allocate A, B, C as `DArray`
   - Fill A, B with deterministic values using `unsafeWrite`
   - Call OpenMP DGEMM via `withPtr` (zero-copy)
   - Demonstrate `split`/`combine` on the output matrix
   - Verify correctness against Phase 7/16 results
   - Benchmark: measure overhead of linear token threading

3. **Update docs** — add Phase 17 to report, README, section numbering

## Success Criteria

1. **Compiles clean** with `-XLinearTypes` on GHC 9.10.3, no external deps
2. **Type safety demonstrated**: trying to read from a split array without
   the right token is a compile-time error
3. **Zero-copy verified**: `split` doesn't allocate or copy — just adjusts
   the offset field. `withPtr` returns a pointer into the shared buffer.
4. **Correctness**: DGEMM results match Phase 7/16 within 1e-6
5. **No runtime overhead**: token threading compiles away — Core shows no
   `MkRW` constructor allocation in hot loops

## Non-Goals

- Generic element types (doubles only, matching Phase 16)
- Nested splits deeper than 1 level (sufficient for row-partitioned DGEMM)
- Automatic parallelism (the split just provides the type-safe partitioning;
  actual parallel execution is via OpenMP as before)
- Full `linear-base` compatibility or API parity with `linear-extra`

## Risks

- `unsafePerformIO` in `unsafeRead`/`unsafeWrite` to bridge linear and IO
  worlds — needs `noDuplicate#` or careful inlining to prevent GHC from
  duplicating effects. Alternative: use `IO` directly and wrap linearity
  around it.
- `-XLinearTypes` may interact unexpectedly with `-O2` optimizations on
  GHC 9.10. Need to verify Core output.
- The CPS-style `alloc` (with rank-2 `forall s.`) ensures the array can't
  escape, but may be awkward to use with multiple arrays. May need a
  `Linearly`-style witness alternative.

## Verified on GHC 9.10.3

The following patterns have been tested and compile cleanly:
- `data RW s where MkRW :: RW s` (GADT with LinearTypes)
- `split :: RW s %1 -> ... -> Slice s` (linear consumption of token)
- `combine :: SlicesTo s l r %1 -> RW l %1 -> RW r %1 -> ... -> RW s`
- `MagicHash` + `LinearTypes` + `UnboxedTuples` together
