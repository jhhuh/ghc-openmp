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

## 2026-02-17: Phase 3 — Lock-free synchronization

Replaced all mutex+condvar with atomics: atomic generation counter, sense-
reversing centralized barrier (Mellor-Crummey & Scott, 1991), spin-wait 4000
iterations + condvar fallback.

Results: fork/join 30x faster (now beats native at 1-4 threads), barriers 28x
faster. Compute-bound work at parity.

**False alarm**: Parallel for showed 1.65x regression at 4 threads — was a
measurement artifact (single-sample benchmark + laptop powersave governor).
Fixed with best-of-10 iterations. Interleaved testing confirmed no regression.

## 2026-02-17: Phase 4 — Haskell calls OpenMP C via FFI

`HsFFIDemo.hs` calls `c_parallel_sinsum` via `foreign import ccall safe`.
Both share GHC Capabilities. Near-linear scaling (3.3x at 4 threads). Key
insight: `hs_init_ghc()` is reference-counted, safe to call when RTS running.

## 2026-02-17: Phase 5 — Concurrent Haskell + OpenMP

Demonstrated `forkIO` threads running alongside OpenMP parallel regions.

**Bug found**: Barrier sense mismatch when `GOMP_parallel` called from
different OS threads (e.g., Haskell `forkIO`). Workers retain stale sense
values from previous region on a different thread → fall through barrier
immediately. Fix: reset all senses in `GOMP_parallel` before dispatching.

## 2026-02-17: Phase 6 — GC interaction

500 short OpenMP regions under GC pressure. Workers are plain OS threads without
Capabilities → invisible to GHC's stop-the-world GC. Worst p99 impact ~1.17x.
CPU frequency scaling dominates variance, not GC.

## 2026-02-17: Phase 7 — Dense matrix multiply (DGEMM)

Naive triple-loop DGEMM validates runtime on real workload. 4.4x speedup at
N=1024 on 4 threads. Haskell reference used `peekElemOff`/`pokeElemOff`.

## 2026-02-17: Phase 8 — DGEMM head-to-head

Same C DGEMM kernel, native libgomp vs RTS-backed runtime. Identical checksums,
overlapping performance distributions. Zero measurable overhead for compute-bound
work confirmed.

## 2026-02-17: Documentation

Created README.md and `docs/report.html` (research report). Set GitHub repo
description and topics via `gh`. Enabled GitHub Pages. Added disclaimers about
tentative results and Claude Code assistance.

## 2026-02-17: Phase 9 — Bidirectional interop

OpenMP workers call Haskell FunPtrs from within `#pragma omp parallel for`.
GHC's `foreign import ccall "wrapper"` stubs handle `rts_lock()/rts_unlock()`
automatically. Per-callback cost ~0.5us. Practical for coarse-grained callbacks.
