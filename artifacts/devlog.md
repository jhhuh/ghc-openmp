# ghc-openmp devlog

## 2026-02-16: Project bootstrap

Set up nix flake with GHC 9.10.3, GCC 15.2.0, Clang, libomp, perf, gdb.
Created planning artifact for GHC-as-OpenMP-runtime exploration.

## 2026-02-16: Phase 1 — Research and ABI stub

**GHC RTS internals**: `cap->no` maps to `omp_get_thread_num()`,
`getNumCapabilities()` maps to `omp_get_num_threads()`, workers pin via
`rts_setInCallCapability()` + `rts_lock()`.

**libgomp ABI**: Full libgomp has ~280 symbols, basic program only needs 9.
Wrote stub `ghc_omp_runtime.c` implementing GOMP_* symbols with pthreads.
Fixed `GOMP_single_start` race (needed per-team counter + startup barrier).

**Decision**: Selected Option D (Hybrid C shim + GHC RTS) — C shim translates
GOMP_* ABI to GHC RTS calls, using one-OS-thread-per-Capability as worker pool.

## 2026-02-17: Phase 2 — RTS-backed runtime

Created `ghc_omp_runtime_rts.c`. Workers are persistent OS threads pinned to
Capabilities. Generation-based dispatch replaces mutex+condvar after debugging
deadlocks:

- 1-thread hang: master waited for `has_work=false` but no workers to set it.
- N-thread deadlock: shared condvar lets worker wake another worker instead of
  master. Redesigned with atomic generation counter.
- Linking: `gcc` can't link GHC shared libs. Used `ghc -threaded -no-hs-main`.

Benchmarks: fork/join 24x slower, barriers 14x slower than native, but
compute-bound parity. Critical sections 1.5-2.4x faster.
