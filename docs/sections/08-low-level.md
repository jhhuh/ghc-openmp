## 7. Low-Level Techniques

This section describes advanced techniques for reducing overhead at the
Haskell-C boundary: zero-overhead Cmm primitives, batched FFI calls,
zero-copy data passing, and linear types for safe mutable array partitioning.

### 7.1 Cmm Primitives

*Source: [`omp_prims.cmm`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_prims.cmm), [`HsCmmDemo.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCmmDemo.hs)*

GHC provides three calling conventions for foreign code, each with different
overhead. We wrote a Cmm primitive that reads `Capability_no(MyCapability())`
— the same value as `omp_get_thread_num()` — to measure the overhead of each
tier (see [Section 8.7](#87-calling-convention-overhead)).

#### The Cmm Primitive

```c
#include "Cmm.h"

omp_prim_cap_no(W_ dummy) {
    return (Capability_no(MyCapability()));
}
```

Called from Haskell via:

```haskell
foreign import prim "omp_prim_cap_no" primCapNo# :: Int# -> Int#
```

The [`inline-cmm`](https://github.com/jhhuh/inline-cmm) library provides a
`[cmm| ... |]` quasiquoter that eliminates the need for separate `.cmm` files
and manual `foreign import prim` declarations — Template Haskell handles
compilation and linking automatically, producing identical zero-overhead results.

#### Key Findings

**Prim calls are truly free**: GHC treats `foreign import prim` functions as
pure expressions. The Cmm function compiles to a single memory load from
`BaseReg`, which GHC can hoist out of loops entirely via LICM (loop-invariant
code motion). In a tight loop, 100M prim calls complete in <1ms.

**The safe FFI tax is ~65ns**: Each `foreign import ccall safe` call costs ~65ns
more than `unsafe` — the price of `suspendThread()`/`resumeThread()` which
release and reacquire the Capability. For OpenMP regions doing >1us of work,
this is negligible (<7%).

**The callback overhead gap**: The ~500ns per callback overhead from
[Section 6.5](#65-bidirectional-callbacks) is ~7x larger than the raw safe FFI
cost (~68ns). The difference comes from `rts_lock()/rts_unlock()` performing
additional work beyond `suspendThread()/resumeThread()`: Task structure
allocation, global Capability search, and lock acquisition. A Cmm-level fast
path could potentially reduce this.

### 7.2 Batched Safe Calls

*Source: [`omp_batch.cmm`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_batch.cmm), [`HsCmmBatch.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCmmBatch.hs)*

The safe FFI tax of ~68ns per call comes from `suspendThread()`/`resumeThread()`,
which release and reacquire the Capability. For workloads that make many short
C calls, this overhead dominates.

By writing the suspend/resume manually in Cmm, we can batch N C calls within
a single Capability release/reacquire cycle, amortizing the ~68ns overhead
across all N calls.

#### The Batching Primitive

```c
#include "Cmm.h"

cmm_batched_tid(W_ n) {
    W_ tok; W_ result; W_ new_base; W_ stack;
    W_ i; W_ t;

    /* Save Sp to TSO — GC needs valid stack pointer */
    stack = StgTSO_stackobj(CurrentTSO);
    StgStack_sp(stack) = Sp;

    (tok) = ccall suspendThread(BaseReg "ptr", 0);

    result = 0; i = 0;
    goto loop_check;
loop_body:
    (t) = ccall omp_get_thread_num();
    result = result + t;
    i = i + 1;
loop_check:
    if (i < n) goto loop_body;

    (new_base) = ccall resumeThread(tok);
    BaseReg = new_base;

    /* Restore Sp — GC may have moved the stack */
    stack = StgTSO_stackobj(CurrentTSO);
    Sp = StgStack_sp(stack);

    return (result);
}
```

#### Implementation Details

Three details were critical for correctness:

1. **Save/restore Sp**: `suspendThread` releases the Capability, allowing GC to
   run. The GC needs a valid `Sp` in the TSO to scan the suspended thread's
   stack. Without this, the GC follows a stale stack pointer and crashes.

2. **No `"ptr"` on tok**: The token from `suspendThread` is `void*`, not a
   GC-traceable pointer. Annotating it as `"ptr"` would tell GHC's Cmm compiler
   to treat it as a GC root, causing the GC to follow a non-heap pointer.

3. **State# threading**: `foreign import prim` is pure by default — GHC can
   CSE or hoist the call. Threading `State# RealWorld` through the type
   signature makes GHC treat it as effectful while adding zero runtime cost
   (State# is erased at the Cmm level).

Benchmark results showing speedups up to 27x are in
[Section 8.8](#88-batched-calls).

### 7.3 Zero-Copy FFI with Pinned ByteArray

*Source: [`HsZeroCopy.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsZeroCopy.hs)*

The standard FFI pattern (`allocaArray` + `peekElemOff`/`pokeElemOff`) boxes
every element as `CDouble` and converts via `realToFrac`, adding overhead at
the Haskell↔OpenMP boundary:

```haskell
-- Boxed: every element goes through CDouble
a <- peekElemOff pA (i * n + k)    -- returns boxed CDouble
b <- peekElemOff pB (k * n + j)    -- returns boxed CDouble
go (acc + realToFrac a * realToFrac b) (k + 1)  -- 4 box/unbox ops
```

Using pinned `ByteArray#` with unboxed primops eliminates this overhead:

```haskell
-- Unboxed: Double# throughout, no boxing
case readDoubleArray# mbaA (i *# n +# k) s of
    (# s', a #) ->     -- a :: Double# (unboxed)
        case readDoubleArray# mbaB (k *# n +# j) s' of
            (# s'', b #) ->     -- b :: Double# (unboxed)
                goK s'' i j (k +# 1#) (acc +## (a *## b))
```

The pinned ByteArray is passed to C via `mutableByteArrayContents#`, which
returns a raw `Addr#` — zero-copy, no marshalling. `touch#` keeps the
ByteArray alive during the C call.

Performance measurements are in [Section 8.9](#89-zero-copy-improvement).

### 7.4 Linear Typed Arrays

*Source: [`Data/Array/Linear.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/Data/Array/Linear.hs), [`HsLinearDemo.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsLinearDemo.hs)*

GHC's `-XLinearTypes` extension can enforce exclusive ownership
of mutable array regions at compile time. The design is inspired by
[`konn/linear-extra`](https://github.com/konn/linear-extra)'s Borrowable
`SArray`, which uses phantom-tagged tokens for zero-copy split/combine. A
self-contained ~200-line module (`Data.Array.Linear`) extracts the core pattern
and integrates it with the unboxed primops from
[Section 7.3](#73-zero-copy-ffi-with-pinned-bytearray)
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
- **Self-contained**: ~200 lines, no dependencies beyond `base` and `ghc-prim`.

---

