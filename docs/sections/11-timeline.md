## 10. Implementation Timeline

The runtime was developed in 21 phases. Each phase is summarized below with
a reference to the section containing full details.

| Phase | Description | Section |
|------:|-------------|---------|
| 1 | Stub pthread-based runtime validating GCC GOMP ABI compatibility | [§4](#4-architecture) |
| 2 | Replace pthreads with GHC Capabilities; hybrid C shim approach | [§4](#4-architecture) |
| 3 | Lock-free optimization: atomic generation counter, sense-reversing barriers | [§5](#5-optimization-from-24x-slower-to-parity) |
| 4 | Haskell FFI interop via `foreign import ccall safe` | [§6.1](#61-ffi-calling-convention) |
| 5 | Concurrent Haskell green threads + OpenMP parallel regions | [§6.3](#63-concurrent-execution) |
| 6 | GC interaction testing — minimal impact on OpenMP latency | [§6.4](#64-garbage-collection-isolation) |
| 7 | Dense matrix multiply (DGEMM) workload | [§9.2](#92-dgemm) |
| 8 | Head-to-head comparison with native libgomp — performance parity | [§9.2](#92-dgemm) |
| 9 | Bidirectional interop: OpenMP workers call Haskell via FunPtr | [§6.5](#65-bidirectional-callbacks) |
| 10 | Cmm primitives via `foreign import prim` — zero-overhead FFI | [§7.1](#71-cmm-primitives) |
| 11 | `inline-cmm` quasiquoter integration | [§7.1](#71-cmm-primitives) |
| 12 | Batched safe calls amortizing 68ns FFI overhead | [§7.2](#72-batched-safe-calls) |
| 13 | Parallelism crossover analysis — break-even at ~500 elements | [§9.4](#94-parallelism-crossover) |
| 14 | GHC native parallelism vs OpenMP — parity with `-fllvm` | [§9.5](#95-ghc-native-parallelism-vs-openmp) |
| 15 | Deferred task execution with work-stealing barriers | [§4.3](#43-task-queues-and-work-stealing) |
| 16 | Zero-copy FFI with pinned ByteArray — 19% inner loop speedup | [§A.6](#a6-zero-copy-ffi-with-pinned-bytearray) |
| 17 | Linear typed arrays for type-safe disjoint partitioning | [§A.7](#a7-linear-typed-arrays) |
| 18 | Runtime improvements: guided scheduling, hybrid barriers, task pools | [§4](#4-architecture) |
| 19 | Shared memory demos: producer-consumer, synchronized, linear | [§8](#8-shared-memory-demos) |
| 20 | Safety demos: overlap bugs, stencil ordering, linear type prevention | [§8](#8-shared-memory-demos) |
| 21 | GHC spark parallelism via `parCombine` with `spark#`/`noDuplicate#` | [§8](#8-shared-memory-demos) |

---

