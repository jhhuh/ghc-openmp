## Implementation Timeline {#sec:timeline}

The runtime was developed in 21 phases. Each phase is summarized below with
a reference to the section containing full details.

| Phase | Description | Section |
|------:|-------------|---------|
| 1 | Stub pthread-based runtime validating GCC GOMP ABI compatibility | [@sec:architecture] |
| 2 | Replace pthreads with GHC Capabilities; hybrid C shim approach | [@sec:architecture] |
| 3 | Lock-free optimization: atomic generation counter, sense-reversing barriers | [@sec:optimization] |
| 4 | Haskell FFI interop via `foreign import ccall safe` | [@sec:ffi-calling-convention] |
| 5 | Concurrent Haskell green threads + OpenMP parallel regions | [@sec:concurrent-execution] |
| 6 | GC interaction testing — minimal impact on OpenMP latency | [@sec:gc-isolation] |
| 7 | Dense matrix multiply (DGEMM) workload | [@sec:dgemm] |
| 8 | Head-to-head comparison with native libgomp — performance parity | [@sec:dgemm] |
| 9 | Bidirectional interop: OpenMP workers call Haskell via FunPtr | [@sec:bidirectional-callbacks] |
| 10 | Cmm primitives via `foreign import prim` — zero-overhead FFI | [@sec:cmm-primitives] |
| 11 | `inline-cmm` quasiquoter integration | [@sec:cmm-primitives] |
| 12 | Batched safe calls amortizing 68ns FFI overhead | [@sec:batched-safe-calls] |
| 13 | Parallelism crossover analysis — break-even at ~500 elements | [@sec:parallelism-crossover] |
| 14 | GHC native parallelism vs OpenMP — parity with `-fllvm` | [@sec:par-compare] |
| 15 | Deferred task execution with work-stealing barriers | [@sec:task-queues] |
| 16 | Zero-copy FFI with pinned ByteArray — 19% inner loop speedup | [@sec:zero-copy-ffi] |
| 17 | Linear typed arrays for type-safe disjoint partitioning | [@sec:linear-arrays] |
| 18 | Runtime improvements: guided scheduling, hybrid barriers, task pools | [@sec:architecture] |
| 19 | Shared memory demos: producer-consumer, synchronized, linear | [@sec:shared-memory] |
| 20 | Safety demos: overlap bugs, stencil ordering, linear type prevention | [@sec:shared-memory] |
| 21 | GHC spark parallelism via `parCombine` with `spark#`/`noDuplicate#` | [@sec:shared-memory] |

---

