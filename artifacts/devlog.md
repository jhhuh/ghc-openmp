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

### Haddock API docs on GitHub Pages
Used `haskellPackages.callCabal2nix` to build Haddock via nixpkgs infra. Output
deployed at `/haddock/` alongside MkDocs site. Mutual links: module header links
to main docs, frontmatter and Haskell integration section link to Haddock.

### Multi-page docs variant
Created `mkdocs-multi.yml` for multi-page MkDocs build. Each section is a separate
page with promoted headings (`##` → `#`). Deployed at `/pages/`. Switcher links
in frontmatter: single-page ↔ multi-page.

Build refactored: substitutions (GIT_COMMIT, #FN: anchors) now run on section files
first, then both single-page (concatenate) and multi-page (copy with heading
promotion) are built from the same resolved sources.

### Cmm syntax highlighting
Changed ```` ```cmm ```` to ```` ```c ```` in `08-low-level.md` since Pygments
doesn't have a Cmm lexer. C highlighting is close enough.

## 2026-02-25: Shared memory docs restructure

Reframed shared memory demos as narrative climax (§15) rather than appendix.
Progressive sequence: Demo 1 (sequential handoff) → Demo 2 (defensive barriers)
→ Demo 3 (linear types eliminate barriers). Each builds on the last.

## 2026-02-26: Fine-partitioning benchmarks — fix real barrier measurement

### GOMP_barrier after parallel region is a no-op

**Critical discovery**: `GOMP_barrier()` called after `GOMP_parallel_end()` is a
no-op — `tl_num_threads=1` (only master thread). Demo 2 was measuring nothing.
Real barriers must be INSIDE `#pragma omp parallel` regions.

**Fix**: Added `transform_partitioned_barrier` and `transform_partitioned_nobarrier`
to `cbits/omp_shared.c`. These create one parallel region and loop over partitions
inside it, with `#pragma omp for schedule(static)` (implicit barrier) or
`#pragma omp for schedule(static) nowait`. Total barriers = iters × num_parts.

Updated Demos 2 and 3 to use these real C partition functions. Extended partition
range to [2..1024]. Demo 3 added Benchmark 3: head-to-head C-barrier vs
C-nobarrier vs Haskell-linear.

## 2026-02-26: Demo 4 — Safety demos (why barriers exist)

### Part A: Off-by-one overlap

Each partition writes [off..off+chunk+1) instead of [off..off+chunk). Boundary
elements written by two partitions. Three variants:
- `c_partitioned_nobarrier` (disjoint) — correct reference
- `c_overlap_barrier` — deterministic but wrong (double-write at boundaries)
- `c_overlap_nowait` — data race (non-deterministic)

**Bug 1: Overlap invisible with `=`**. Both partitions compute the same `f(in[i])`
for boundary elements. Assignment is idempotent: `out[i] = f(in[i])` twice gives
the same result. Fix: changed to `+=` with zero-initialization. Boundary elements
accumulate `2*f(x)`, making the double-write detectable.

### Part B: Two-pass stencil

Pass 1: `out[i] = f(in[i])` (independent). Pass 2: `out[i] = avg(out[i-1..i+1])`
(reads neighbors). Without barrier between passes, pass 2 reads stale/zero values.

**Bug 2: Race not triggering with `schedule(static)`**. Static scheduling assigns
the same elements to each thread in both passes. The race window is near zero —
thread T finishes pass 1 and starts pass 2 on the same elements, which are already
written. Fix: `schedule(dynamic, 64)` varies element-to-thread assignment across
passes, reliably triggering stale reads (max diff = 1.09e-2).

**Bug 3: Haskell linear showing huge errors (3.0 to 31.7)**. Base-offset bug in
`linearMultiPass1`: when output array is a right-half slice with offset=n/2,
`linearTransform` reads `arrIn[0..n/2-1]` instead of `arrIn[n/2..n-1]`. Fix:
added `linearTransformAt` with explicit `base` parameter, threaded through
recursive splitting. Same bug existed in Demo 3's `linearMultiPartition` — fixed.

**Bug 4: C stencil vs Haskell reference mismatch (4e-7 even with barriers)**.
C parallel stencil is Jacobi-style (reads original neighbor values). Haskell
sequential is Gauss-Seidel (reads already-updated left neighbors). These are
fundamentally different algorithms. Fix: compare apples-to-apples — C nowait vs
C barrier (both Jacobi), Haskell linear vs Haskell ref (both Gauss-Seidel).

### linearPass2 compilation error

`let (cur, rw1) = unsafeRead rw arr i` consumed the linear token for a read,
leaving no token for subsequent writes. Error: "Couldn't match type 'Many' with
'One'". Fix: use fabricated `MkRW` for ALL reads (which don't actually consume
ownership), only consume real `rw` through `unsafeWrite`.

### Final results

Part A: Disjoint=0 CORRECT, Overlap+barrier=3-31 WRONG, Overlap+nowait=3-31 WRONG.
Part B: C nowait=1.09e-2 WRONG (stale reads), Haskell linear=0 CORRECT.

## 2026-02-26: Research on konn/linear-extra

Investigated `github.com/konn/linear-extra` for improvement ideas.

**Key architectural differences from our `Data.Array.Linear`**:
1. Separated R/W tokens: `R s` (read) + `W s` (write) vs our monolithic `RW s`
2. Non-Consumable tokens via `Unsatisfiable` — prevents accidental token discard
3. `combine` returns parent `DArray` (not just token) — prevents use-after-combine
4. Zero-bit `SlicesTo` via `UnliftedNewtypes` over `Void#`
5. `noDuplicate#` for correctness with `par` — prevents GHC from duplicating
   thunks containing mutations
6. Storable-based vs our MutableByteArray#-based
7. `freeze` produces `Vector` in O(1) vs our O(n) list copy
8. `besides` mechanism for allocating alongside existing linear values
9. Linear `par` using `spark#` + `noDuplicate#`

**Discourse post**: konn's FFT experience report shows ThreadScope multi-core
visualization. `linear-fft`: Pure, parallel, in-place FFT using linear types.
`linear-parallel`: Linear par/seq combinators.

### Proposed improvements (approved by user)

1. Add `par` combinator using `spark#` + `noDuplicate#` to `Data.Array.Linear`
2. Switch from `unsafeDupablePerformIO` to `noDuplicate#` in write operations
3. Mark `split`, `combine`, `halve` as `NOINLINE`
4. Create Demo 5: Parallel Prefix Sum using new `par` combinator

## 2026-02-26: Implement parCombine and Demo 5

### Data.Array.Linear improvements

Added to `demos/Data/Array/Linear.hs`:
1. `NOINLINE` pragmas on `split`, `combine`, `halve` — prevents GHC from
   inlining and accidentally allowing token misuse
2. `parCombine` — like `combine` but sparks the left token and evaluates the
   right on the current thread. Uses `spark#` + `seq#` via `unsafePerformIO`
   (which includes `noDuplicate#`)

Implementation detail: `parCombine` uses `unsafeCoerce#` to bridge linear
types with non-linear GHC spark primitives. This is the same approach as
`konn/linear-extra`'s `Unsafe.toLinear`. The linearity contract is upheld:
each token is consumed exactly once (sparked or sequenced).

Kept `unsafeDupablePerformIO` for individual `unsafeRead`/`unsafeWrite` —
these are protected by linear token consumption, and the faster dupable
variant is appropriate. Only `parCombine` needs `noDuplicate#` for the
spark+eval sequence.

### Demo 5: GHC Spark Parallelism

Pure Haskell parallelism via GHC sparks + linear types. Same transform
workload as Demos 2–4 but no C/OpenMP. Recursive split + parCombine.

Results on 4 capabilities:
```
Depth  Parts  Sequential  Parallel  Speedup
0      1       41.9 ms    43.6 ms   0.96x
1      2       44.4 ms    22.3 ms   1.99x  ← near-ideal
2      4       42.5 ms    13.8 ms   3.09x  ← near-ideal on 4 cores
3      8       42.1 ms    15.0 ms   2.80x
5      32      46.1 ms    13.5 ms   3.41x
6      64      45.7 ms    13.7 ms   3.34x
```

Array size scaling (depth=3, 8 partitions):
```
N         Sequential  Parallel  Speedup
10K        0.4 ms     0.4 ms    0.97x
100K       4.4 ms     3.0 ms    1.49x
1M        43.3 ms    17.0 ms    2.55x
4M       164.6 ms    53.3 ms    3.09x
```

This completes the demo progression:
- Demo 1: Sequential handoff (no concurrency)
- Demo 2: Concurrent with defensive barriers
- Demo 3: Linear types eliminate barriers (Haskell seq + C parallel)
- Demo 4: Safety — why barriers exist
- Demo 5: Pure Haskell parallelism via sparks + linear types
