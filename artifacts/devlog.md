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

## 2026-02-22: Phase 10 — Cmm primitives benchmark

Wrote `omp_prims.cmm` — single memory load from `BaseReg` for Capability
number. Results: prim ~0ns (optimized away by GHC), unsafe FFI ~2.3ns, safe FFI
~67ns (29x ratio). Prim being pure means GHC does LICM; needed data-dependent
loop to measure.

## 2026-02-22: Phase 11 — inline-cmm integration

Integrated `jhhuh/inline-cmm` via cabal source-repository-package. The
`[cmm| ... |]` quasiquoter automates compilation and linking. Same ~0ns
overhead as hand-written Cmm.

## 2026-02-22: Phase 12 — Batched safe calls via Cmm

Manual `suspendThread`/`resumeThread` in Cmm amortizes 65ns overhead across
N calls. At batch=100, per-call cost drops to 2.7ns (approaching unsafe FFI).

**Three bugs found and fixed**:
1. Missing `Sp` save/restore before `suspendThread` → GC scans garbage → segfault.
2. `"ptr"` annotation on `resumeThread` token → treated as GC root → crash.
3. `foreign import prim` is pure → GHC CSEs side effects. Fix: thread
   `State# RealWorld`.

## 2026-02-23: Phase 13 — Parallelism crossover

OpenMP from Haskell breaks even at ~500 elements (for ~11ns/element work).
Fixed overhead ~1.8us (86ns safe FFI + 1712ns fork/join). Near-ideal 4x
scaling at 100K+ elements.

## 2026-02-23: Phase 14 — GHC native parallelism vs OpenMP

`forkIO` + manual splitting vs OpenMP safe FFI. OpenMP consistently ~2x faster.
Gap is per-element cost (GCC SIMD + no boxing), not parallelism efficiency.

## 2026-02-23: Phase 15 — Deferred task execution

Global task queue + work-stealing barriers. Near-linear speedup (3.4-4.0x on
4 threads). 1.7x overhead vs native libgomp (mutex queue vs lock-free).

**Key discovery**: GCC may omit explicit `GOMP_barrier` after `#pragma omp
single`. Task stealing must happen at pool end_barrier, not just
`GOMP_barrier`. Also: GCC passes `cpyfn=NULL` for simple scalar firstprivate.

## 2026-02-23: Phase 16 — Zero-copy FFI with pinned ByteArray

`newPinnedByteArray#` + `mutableByteArrayContents#` for zero-copy data
passing. `readDoubleArray#`/`writeDoubleArray#` eliminate CDouble boxing.
Inner loop 19% faster at N=512. Verified via `-ddump-simpl`.

**Gotcha**: `forM_ [0..n-1]` allocates a list — GHC doesn't always fuse it.
Replaced with manual `go` loop.

## 2026-02-23: Phase 17 — Linear typed arrays

Self-contained `Data.Array.Linear` (~200 lines) inspired by
`konn/linear-extra` Borrowable SArray. `RW s` linear tokens + zero-copy
`split`/`combine` via offset arithmetic. Tiled DGEMM demo.

**Bugs**:
1. `runRW#` creates independent state threads → writes not sequenced.
   Fix: `unsafeDupablePerformIO`.
2. Underscore-prefixed `let` bindings lazy → fills never execute. Fix: `seq`.
3. `withPtr` via `runRW#` doesn't propagate C side effects.
   Fix: `unsafeWithPtr` in IO.

## 2026-02-23: Documentation improvements

Added Mermaid charts for 5 key benchmarks + architecture diagram. Fixed
`\\n` → `<br/>`, invisible `~~~` links for horizontal layout. Cited
Mellor-Crummey & Scott (1991). Credited `konn/linear-extra`.

## 2026-02-24: Related Work

Added section citing BOLT (OpenMP over Argobots) and Argobots. Comparison table
shows architectural parallel (ES↔Capability, ULT↔green thread). Key difference:
purpose-built HPC substrate vs repurposed language runtime.

## 2026-02-24: Phase 18 — Five runtime improvements

Inspired by BOLT/Argobots:
1. True guided scheduling (CAS loop, exponentially-decreasing chunks).
2. Hybrid spin-sleep barriers (`OMP_WAIT_POLICY` support).
3. Pre-allocated task descriptor pool (no per-task malloc/free).
4. Per-Capability task queues with work stealing.
5. Serialized nested parallelism with level tracking.

## 2026-02-24: Fix barrier overhead

`spin_barrier_wait_tasks` was doing task stealing even when no tasks pending,
adding overhead to every barrier. Fix: check `g_tasks_pending > 0`.

## 2026-02-24: Nix build support

Added `packages.default` and `apps` (test-all, bench, bench-dgemm, test-tasks)
to `flake.nix`. Binary builds via `nix build`, runners via `nix run .#test-all`.

## 2026-02-24: Restructure docs from chronological to thematic

Reorganized `docs/index.md` from 19 chronological phase-based sections to 13
thematic sections. All content preserved. Collected scattered benchmark data
into unified section.

## 2026-02-24: Replace Jekyll + pandoc with Hakyll

**Problem**: Pandoc strips leading numbers from heading IDs, breaking anchor
links. Jekyll/kramdown keeps them.

**Solution**: Hakyll with `pandocCompilerWithTransform` + custom
`fixHeadingIds` walking Pandoc AST. GitHub Actions deploys via `nix build .#docs`.

**Gotchas**:
1. Hakyll in nix needs `glibcLocales` + `LANG=en_US.UTF-8`.
2. `installPhase` needs `mkdir -p $out` before `cp -r _site/* $out/`.
3. Switched Pages from branch-based Jekyll to Actions deployment.

## 2026-02-25: Packaging, benchmarks, docs split

### libghcomp.so with RTS linkage
Built `libghcomp.so` from `ghc_omp_runtime_rts.c` (PIC) + `HsStub.hs` (dynamic).
Key insight: GHC's shared libraries don't list `libHSrts_thr.so` as a dependency.
Must explicitly link it and embed rpath:
```
-optl-L$(RTS_SODIR) -optl-l$(RTS_LIBNAME) -optl-Wl,-rpath,$(RTS_SODIR)
```
Plain `gcc -fopenmp test.c -lghcomp` now works — GHC RTS boots transparently.

### Cabal package
Created `ghc-openmp.cabal` with `c-sources: src/ghc_omp_runtime_rts.c`. Haskell
consumers compile the runtime with their own GHC (no ABI conflicts). Moved
`inline-cmm-demo.cabal` to `demos/inline-cmm/` to avoid multiple-cabal-file error.
`lib/GHC/OpenMP.hs` has full Haddock coverage (100%).

### Benchmark infrastructure
`nix run .#benchmark` runs all benchmarks, captures:
- System info → `artifacts/bench/system_info.json`
- Raw logs → `artifacts/bench/logs/*.log`
- Parsed results → `artifacts/bench/results.json`
- Markdown tables → `artifacts/bench/summary.md`

Bash gotcha: `local a="$1" b="$2" c="$a"` — `$a` in third assignment uses the
PRE-local value, not the one just assigned. Must split `local` declarations.

### Docs split
Split 1388-line `docs/index.md` into 19 files under `docs/sections/`. Build-time
concatenation via `cat docs/sections/*.md > docs/index.md` in flake.nix. Added
`docs/index.md` to `.gitignore`. MkDocs `exclude_docs: sections/` prevents
double-processing.

### Architecture diagram
Added `...` / `Cap N-1` ellipses to ASCII art (README.md) and Mermaid (index.md).

### Sense-reversing barrier appendix
New `docs/sections/A5-barrier.md` explaining Mellor-Crummey & Scott barrier,
`spin_barrier_t`, hybrid spin-wait, task-stealing variant.
