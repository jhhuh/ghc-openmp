# Phase 10: Cmm Primitives — Calling Convention Benchmark

## Setup

- GHC 9.10.3, GCC 15.2.0, Intel i7-10750H, NixOS, powersave governor
- Cmm primitive `omp_prim_cap_no` reads `Capability_no(MyCapability())` — a single memory load from BaseReg
- Compared against `omp_get_thread_num()` via unsafe and safe FFI
- Best-of-5 interleaved runs, pointer-based loops to avoid boxing noise

## Results (consistent across 3 runs)

| Calling Convention | ns/call | Relative | Notes |
|---|---|---|---|
| `foreign import prim` (Cmm) | ~0 | 1x | Pure call, GHC optimizes away entirely |
| `foreign import ccall unsafe` | 2.3 | — | STG register save/restore |
| `foreign import ccall safe` | 67.5 | 29x vs unsafe | + suspendThread/resumeThread |

## Key Findings

1. **Prim calls are truly free**: GHC treats `foreign import prim` as a pure
   expression. When loop-invariant, it's hoisted out entirely. Even with data
   dependencies, the per-call cost is sub-nanosecond (single memory load).

2. **Unsafe FFI ~2ns**: The C calling convention switch (save/restore STG
   registers) costs about 2 nanoseconds per call.

3. **Safe FFI ~68ns**: The `suspendThread()`/`resumeThread()` Capability
   release/reacquire cycle adds ~65ns. This is the cost of:
   - Saving thread state to TSO
   - Releasing the Capability
   - Making the C call
   - Reacquiring the Capability
   - Restoring thread state

4. **Safe/unsafe ratio: 29x**: For trivial functions, the safe FFI overhead
   is significant. For functions doing >1us of work, it's negligible (<7%).

## Implications for ghc-openmp

- **Forward path (Haskell → OpenMP C)**: The ~68ns safe FFI overhead per call
  is negligible for typical OpenMP regions (>1us). No optimization needed.

- **Callback path (OpenMP → Haskell via FunPtr)**: The ~500ns per callback
  overhead (from Phase 9) is ~7x larger than the raw safe FFI cost. The
  extra ~430ns comes from `rts_lock()/rts_unlock()` doing more work than
  `suspendThread()/resumeThread()` (Task allocation, Capability search).

- **Cmm primitives for RTS queries**: Functions like `omp_get_thread_num()`
  could be replaced with Cmm prims for zero-cost access from Haskell code.
  Useful for polling or high-frequency queries, not needed for OpenMP dispatch.

## Files

- `src/omp_prims.cmm` — Cmm primitive reading `Capability_no`
- `src/HsCmmDemo.hs` — Benchmark driver
