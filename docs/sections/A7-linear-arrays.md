## A.7 Linear Typed Arrays

*Source: [`Data/Array/Linear.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/Data/Array/Linear.hs), [`HsLinearDemo.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsLinearDemo.hs)*

GHC's `-XLinearTypes` extension can enforce exclusive ownership
of mutable array regions at compile time. The design is inspired by
[`konn/linear-extra`](https://github.com/konn/linear-extra)'s Borrowable
`SArray`, which uses phantom-tagged tokens for zero-copy split/combine. A
self-contained ~200-line module (`Data.Array.Linear`) extracts the core pattern
and integrates it with the unboxed primops from
[Appendix A.6](#a6-zero-copy-ffi-with-pinned-bytearray)
(`readDoubleArray#`/`writeDoubleArray#` instead of `Storable`).

The core idea: linearity is on *tokens* (`RW s`), not the array itself. You
need the right token to read/write, and `split`/`combine` tracks disjoint
ownership at the type level.

#### Design

```haskell
-- Linear token: proves exclusive access to region s
data RW s where MkRW :: RW s

-- Array with phantom region (NOT linear — tokens enforce access)
data DArray s = DArray !Int !(MutableByteArray# RealWorld) !Int  -- len, buf, offset

-- Split witness: proves l and r came from splitting s
data SlicesTo s l r where MkSlicesTo :: SlicesTo s l r

-- Operations consume and return tokens
unsafeRead  :: RW s %1 -> DArray s -> Int -> (Double, RW s)
unsafeWrite :: RW s %1 -> DArray s -> Int -> Double -> RW s
split       :: RW s %1 -> Int -> DArray s -> Slice s
combine     :: SlicesTo s l r %1 -> RW l %1 -> RW r %1 -> RW s
```

The `split` operation is zero-copy — both halves share the same underlying
`MutableByteArray#`, just with different offset/length views. No allocation,
no copying, just arithmetic on the offset field.

#### Type-Safe Row-Partitioned DGEMM

```haskell
case split rwC (half * n) arrC of
    MkSlice st rwTop rwBot cTop cBot ->
        let rwTop' = linearDgemm rwTop n 0    half arrA arrB cTop
            rwBot' = linearDgemm rwBot n half half arrA arrB cBot
            rwC'   = combine st rwTop' rwBot'
        in  unsafeRead rwC' arrC 0  -- can only read after recombining
```

This is the Haskell-side type encoding of what OpenMP does at runtime with
`#pragma omp parallel for schedule(static)` — partitioning the output matrix
into disjoint row blocks. The type system statically guarantees:

1. No two computations can write to the same rows
2. The original array cannot be accessed while split
3. All slices must be recombined before reading results

#### C FFI Integration

The `unsafeWithPtr` function passes pinned ByteArray data directly to C:

```haskell
unsafeWithPtr :: DArray s -> (Ptr CDouble -> IO a) -> IO a
```

This threads through IO properly (unlike `runRW#`-based approaches that
create independent state threads), ensuring C side effects are visible to
subsequent Haskell reads.

#### Implementation Notes

- **`unsafeDupablePerformIO` over `runRW#`**: Element access uses
  `unsafeDupablePerformIO` with `{-# NOINLINE #-}` to ensure writes from one
  operation are visible to subsequent reads. `runRW#` creates independent state
  threads that GHC can reorder under `-O2`.
- **Bang patterns for sequencing**: In lazy contexts, token-producing
  operations must be forced with `!` to ensure their side effects execute.
- **`parCombine` for GHC sparks**: A parallel variant of `combine` that sparks
  the left token for parallel evaluation using `spark#`/`seq#`. Uses
  `unsafePerformIO` (which includes `noDuplicate#`) to prevent thunk
  duplication — essential when tokens are produced by destructive array
  operations. `unsafeCoerce#` bridges linear types with non-linear GHC
  primitives (same approach as
  [`konn/linear-extra`](https://github.com/konn/linear-extra)'s
  `Unsafe.toLinear`).
- **Self-contained**: ~280 lines, no dependencies beyond `base` and `ghc-prim`.

---

