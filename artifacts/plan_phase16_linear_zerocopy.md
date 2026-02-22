# Plan: Phase 16 — Zero-Copy FFI with Linear Types

## Goal
Eliminate boxing overhead at the Haskell↔OpenMP boundary using linear types
and unboxed array representations. Demonstrate that Haskell can pass data to
OpenMP C code with zero marshalling cost — matching C-to-C performance.

## Problem Statement

Current Haskell drivers (Phases 4, 7, 9) have three sources of overhead
compared to pure C OpenMP:

| Pattern | Location | What happens | Cost |
|---------|----------|--------------|------|
| List-to-Ptr marshalling | `HsMain.hs:30-32` `withArray'` | `map (CDouble . f) [0..n-1]` builds a lazy list of boxed CDoubles, then `pokeArray` writes them one-by-one | O(n) allocation + GC pressure |
| Per-element boxing | `HsMatMul.hs:40-42` inner loop | Each `peekElemOff` returns boxed `CDouble`, `realToFrac` converts to boxed `Double` | 2 box/unbox per element per iteration |
| Callback boxing | `HsCallback.hs` | Each `callback(i)` acquires Capability, boxes argument, unboxes result | ~68ns per element (safe FFI) |

In pure C, the equivalent code touches raw `double*` with no boxing at all.

## Approach: `linear-extra` SArray + Pinned ByteArray

### What SArray provides
`linear-array-extra`'s `SArray a` is:
- Off-heap, `Ptr`-based (not moved by GC)
- C-compatible memory layout (contiguous doubles)
- Linear ownership (`%1 ->`) — compile-time use-after-free prevention
- `split`/`combine` with `SlicesTo` tokens — type-safe sub-array views

### What we'll actually build

Rather than taking the full `linear-extra` dependency immediately (it has
its own dependency chain and may need patches for GHC 9.10), we'll build a
minimal demonstration using GHC's existing primitives that proves the concept:

**Step 1**: Pinned `ByteArray#` zero-copy DGEMM
- Use `newPinnedByteArray#` to allocate matrices
- Fill them using `writeDoubleArray#` (no boxing)
- Pass to OpenMP C code via `byteArrayContents#` (zero-copy, no marshalling)
- Read results back via `indexDoubleArray#` (no boxing)
- Compare wall-clock time against current `allocaArray`/`peekElemOff` approach

**Step 2**: Unboxed inner loop
- Replace the `peekElemOff`/`realToFrac` pattern in `haskellDgemm` with
  direct `indexDoubleOffAddr#` primop calls
- Eliminate all intermediate `CDouble` boxing in the hot loop
- Benchmark: time per iteration reduction

**Step 3**: LinearTypes ownership sketch (optional, depends on GHC support)
- If `LinearTypes` works cleanly with our GHC version, demonstrate a
  linear wrapper around the pinned ByteArray that prevents aliasing
- Show how `split`/`combine` maps to OpenMP data-sharing semantics
- This is a proof-of-concept, not a production API

## Architecture

```
Current (Phase 7):
  Haskell [Double] → pokeArray → Ptr CDouble → C OpenMP → peekElemOff → [Double]
                     ↑ O(n) copy + boxing                  ↑ per-element boxing

Phase 16:
  Haskell ByteArray# (pinned) → byteArrayContents# → Addr# → C OpenMP
                                ↑ zero-copy, no allocation
  Read back: indexDoubleArray# → Double# (unboxed, no boxing)
```

## Files

### New files
- `src/HsZeroCopy.hs` — Phase 16 demo: zero-copy DGEMM with pinned ByteArray
- `artifacts/results_phase16_zerocopy.md` — benchmark results

### Modified files
- `Makefile` — add `build/zerocopy_demo` and `demo-zerocopy` targets
- `README.md` — add Phase 16 to project structure and demo targets

## Implementation Details

### HsZeroCopy.hs outline

```haskell
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Main where

import GHC.Exts
import GHC.IO
import GHC.ForeignPtr (ForeignPtr)
import Foreign.C
import GHC.Clock

-- Same C FFI as HsMatMul:
foreign import ccall safe "parallel_dgemm"
    c_parallel_dgemm :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO ()

-- Allocate pinned, fill without boxing, pass to C without copying
main = do
    -- 1. Allocate pinned ByteArray# (n*n doubles)
    -- 2. Fill via writeDoubleArray# in a loop (no CDouble boxing)
    -- 3. Get Addr# via byteArrayContents# (zero-copy)
    -- 4. Call c_parallel_dgemm with Ptr (Addr#)
    -- 5. Read results via indexDoubleArray# (no boxing)
    -- 6. Compare timing against allocaArray approach
```

### Key primops used

| Primop | Purpose |
|--------|---------|
| `newPinnedByteArray#` | Allocate GC-managed but non-moving memory |
| `byteArrayContents#` | Get raw `Addr#` — zero-copy pointer to C |
| `writeDoubleArray#` | Write `Double#` without `CDouble` boxing |
| `indexDoubleArray#` / `indexDoubleOffAddr#` | Read `Double#` without boxing |
| `touch#` | Keep ByteArray alive during C call |

### Build target

```makefile
build/zerocopy_demo: src/HsZeroCopy.hs build/omp_compute.o build/ghc_omp_runtime_rts.o
	@mkdir -p build
	$(GHC) -threaded -O2 \
		src/HsZeroCopy.hs build/omp_compute.o build/ghc_omp_runtime_rts.o \
		-o $@ -lpthread -lm \
		-outputdir build/hs_zerocopy_out
```

## Success Criteria

1. **Zero-copy verified**: No `mallocArray`/`pokeArray` in the hot path.
   `byteArrayContents#` gives C a direct pointer to pinned Haskell memory.

2. **Boxing eliminated**: Inner loop uses `Double#` throughout — no
   intermediate `CDouble` or boxed `Double` allocations. Verifiable via
   `-ddump-simpl` showing unboxed worker.

3. **Performance measurable**: Wall-clock improvement over Phase 7's
   `allocaArray`/`peekElemOff` approach on N=512 and N=1024 DGEMM.
   Expected: setup time (allocation + fill) drops significantly;
   compute time unchanged (same C kernel).

4. **Correctness**: Results match Phase 7 within 1e-6.

## Non-Goals

- Full linear-types API design (that's a future phase)
- SIMD vectorization of the Haskell sequential DGEMM (separate phase)
- Replacing the C OpenMP DGEMM itself
- Supporting arbitrary element types (doubles only for now)

## Risks

- `byteArrayContents#` requires `touch#` after the C call to prevent
  premature GC. Need to handle this correctly.
- Pinned ByteArrays contribute to heap fragmentation. For benchmark-sized
  matrices (1024x1024 = 8MB), this is fine.
- `LinearTypes` extension may have rough edges with `MagicHash` on GHC 9.10.
  Step 3 is optional for this reason.

## Verification Plan

1. Build `demo-zerocopy` → runs without crash
2. Results match `demo-matmul` (Phase 7) within 1e-6
3. Timing output shows setup time reduction
4. `-ddump-simpl` confirms unboxed `Double#` in inner loop (no `D#` constructor)
