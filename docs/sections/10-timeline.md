## 9. Implementation Timeline

The runtime was developed in 18 phases. Each phase is summarized below with
a reference to the section containing full details.

| Phase | Description | Section |
|------:|-------------|---------|
| 1 | Stub pthread-based runtime validating GCC GOMP ABI compatibility | [§4](#4-architecture) |
| 2 | Replace pthreads with GHC Capabilities; hybrid C shim approach | [§4](#4-architecture) |
| 3 | Lock-free optimization: atomic generation counter, sense-reversing barriers | [§5](#5-optimization-from-24x-slower-to-parity) |
| 4 | Haskell FFI interop via `foreign import ccall safe` | [§6.1](#61-ffi-calling-convention) |
| 5 | Concurrent Haskell green threads + OpenMP parallel regions | [§6.3](#63-concurrent-execution) |
| 6 | GC interaction testing — minimal impact on OpenMP latency | [§6.4](#64-garbage-collection-isolation) |
| 7 | Dense matrix multiply (DGEMM) workload | [§8.2](#82-dgemm) |
| 8 | Head-to-head comparison with native libgomp — performance parity | [§8.2](#82-dgemm) |
| 9 | Bidirectional interop: OpenMP workers call Haskell via FunPtr | [§6.5](#65-bidirectional-callbacks) |
| 10 | Cmm primitives via `foreign import prim` — zero-overhead FFI | [§7.1](#71-cmm-primitives) |
| 11 | `inline-cmm` quasiquoter integration | [§7.1](#71-cmm-primitives) |
| 12 | Batched safe calls amortizing 68ns FFI overhead | [§7.2](#72-batched-safe-calls) |
| 13 | Parallelism crossover analysis — break-even at ~500 elements | [§8.4](#84-parallelism-crossover) |
| 14 | GHC native parallelism vs OpenMP — parity with `-fllvm` | [§8.5](#85-ghc-native-parallelism-vs-openmp) |
| 15 | Deferred task execution with work-stealing barriers | [§4.3](#43-task-queues-and-work-stealing) |
| 16 | Zero-copy FFI with pinned ByteArray — 19% inner loop speedup | [§7.3](#73-zero-copy-ffi-with-pinned-bytearray) |
| 17 | Linear typed arrays for type-safe disjoint partitioning | [§7.4](#74-linear-typed-arrays) |
| 18 | Runtime improvements: guided scheduling, hybrid barriers, task pools | [§4](#4-architecture) |

---

