## 8. Benchmarks

All benchmarks on an Intel i7-10750H (6C/12T), NixOS, GCC 15.2, GHC 9.10.3,
powersave governor. Best-of-N timing to reduce CPU frequency variance.

### 8.1 Microbenchmarks

*Source: [`bench_overhead.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/bench_overhead.c)*

<!-- BENCH:overhead_forkjoin -->
#### Fork/Join Overhead (us/iter)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 0.171 | 0.030 | 5.7x faster |
| 2 | 0.828 | 0.420 | 2.0x faster |
| 4 | 0.972 | 0.811 | 1.2x faster |
| 8 | 1.346 | 1.517 | 1.13x |
<!-- /BENCH:overhead_forkjoin -->

<!-- BENCH:overhead_barrier -->
#### Barrier Latency (us/iter)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 0.026 | 0.002 | 13x faster |
| 2 | 0.313 | 0.136 | 2.3x faster |
| 4 | 0.508 | 0.254 | 2.0x faster |
| 8 | 0.762 | 0.482 | 1.6x faster |
<!-- /BENCH:overhead_barrier -->

<!-- BENCH:overhead_parfor -->
#### Parallel For + Reduction (1M sin(), best of 10, ms)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 15.764 | 15.427 | 0.98x |
| 2 | 7.730 | 7.783 | 1.01x |
| 4 | 3.849 | 3.905 | 1.01x |
| 8 | 3.358 | 3.503 | 1.04x |
<!-- /BENCH:overhead_parfor -->

<!-- BENCH:overhead_critical -->
#### Critical Section (1000 lock/unlock per thread, ms)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 0.054 | 0.026 | 2.1x faster |
| 2 | 0.318 | 0.254 | 1.3x faster |
| 4 | 0.915 | 0.381 | 2.4x faster |
| 8 | 2.135 | 1.201 | 1.8x faster |
<!-- /BENCH:overhead_critical -->

### 8.2 DGEMM

*Source: [`bench_dgemm.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/bench_dgemm.c), [`omp_compute.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_compute.c)*

Same naive triple-loop DGEMM compiled identically, linked against either
native libgomp or our runtime. Checksums match exactly.

#### 4 Threads

<!-- BENCH:dgemm_4t -->
| N | Native (ms) | RTS (ms) | Ratio | GFLOPS (RTS) |
|---|---|---|---|---|
| 128 | 0.64 | 0.77 | 1.20x | 5.47 |
| 256 | 9.47 | 10.22 | 1.08x | 3.28 |
| 512 | 66.11 | 68.05 | 1.03x | 3.94 |
| 1024 | 603.84 | 536.88 | 0.89x | 4.00 |
<!-- /BENCH:dgemm_4t -->

Interleaved re-runs confirm the two runtimes trade leads: the difference is
CPU frequency noise, not runtime overhead.

<figure>
<img src="charts/dgemm-comparison.svg" alt="DGEMM Head-to-Head: Native libgomp vs RTS" />
</figure>

<!-- BENCH:dgemm_scaling -->
#### Scaling (RTS-backed, DGEMM 1024x1024)

| Threads | Time (ms) | GFLOPS | Speedup |
|---|---|---|---|
| 1 | 2434.99 | 0.88 | 1.0x |
| 2 | 1330.19 | 1.61 | 1.8x |
| 4 | 663.37 | 3.24 | 3.7x |
<!-- /BENCH:dgemm_scaling -->

### 8.3 FFI Scaling

*Source: [`HsMain.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsMain.hs)*

Haskell calling parallel sinsum via safe FFI:

| Threads | Time (ms) | Speedup |
|---|---|---|
| 1 | 32.5 | 1.0x |
| 2 | 17.3 | 1.9x |
| 4 | 9.9 | 3.3x |
| 8 | 5.0 | 6.5x |

Near-linear scaling through the FFI boundary, confirming the runtime
correctly parallelizes work dispatched from Haskell.

### 8.4 Parallelism Crossover

*Source: [`HsCrossover.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCrossover.hs)*

When does OpenMP from Haskell beat sequential C? We measured sinsum
(compute-bound, ~11ns per element) at various sizes with 4 threads.

<!-- BENCH:crossover -->
| Elements | Sequential | Parallel | Speedup |
|---|---|---|---|
| 100 | 0.5 us | 2.1 us | 0.26x |
| 200 | 1.3 us | 2.2 us | 0.57x |
| 500 | 3.6 us | 2.9 us | **1.23x** |
| 1000 | 7.5 us | 3.8 us | **1.99x** |
| 5000 | 48.1 us | 16.5 us | **2.91x** |
| 100000 | 1100.9 us | 290.0 us | **3.80x** |
<!-- /BENCH:crossover -->

The crossover is at **~500 elements** — above this, OpenMP parallel execution
from Haskell is faster than sequential C called via unsafe FFI. The fixed
overhead is ~1.8us (86ns safe FFI + 1712ns OpenMP fork/join).

<figure>
<img src="charts/crossover.svg" alt="Parallelism Crossover: Sequential vs Parallel" />
</figure>

### 8.5 GHC Native Parallelism vs OpenMP

*Source: [`HsParCompare.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsParCompare.hs)*

For the same compute-bound sinsum workload, how does Haskell's `forkIO` with
manual work splitting compare to OpenMP via safe FFI?

#### GHC NCG (default code generator)

<!-- BENCH:par_compare -->
| Elements | Seq Haskell | Seq C | Par Haskell | Par OpenMP | Hs/OMP ratio |
|---|---|---|---|---|---|
| 10K | 233.6 us | 105.8 us | 77.8 us | 51.1 us | 1.52x |
| 100K | 2472.0 us | 1104.6 us | 634.9 us | 515.4 us | 1.23x |
| 1M | 24136.5 us | 11088.9 us | 6094.4 us | 5037.8 us | 1.21x |
| 10M | 241447.8 us | 111060.6 us | 60838.7 us | 30872.0 us | 1.97x |
<!-- /BENCH:par_compare -->

With the default NCG backend, OpenMP is consistently **~2x faster** than
parallel Haskell. Sequential Haskell is also ~2.2x slower than sequential C.
The gap comes entirely from per-element code quality, not parallelism overhead
— both achieve near-ideal scaling on 4 threads.

#### GHC LLVM backend (`-fllvm`)

| Elements | Seq Haskell | Seq C | Par Haskell | Par OpenMP | Hs/OMP ratio |
|---|---|---|---|---|---|
| 10K | 104 us | 103 us | 43 us | 28 us | 1.5x |
| 100K | 1109 us | 1101 us | 303 us | 278 us | 1.1x |
| 1M | 11156 us | 10998 us | 2972 us | 2866 us | **1.04x** |
| 10M | 111751 us | 111112 us | 30924 us | 28747 us | **1.08x** |

Compiling with `-fllvm` (LLVM 20.1) **eliminates the gap entirely**. Sequential
Haskell matches sequential C, and parallel Haskell reaches parity with OpenMP.
The 2x gap under NCG is purely a code generator limitation — GHC's inner loop
compiles to 17 instructions vs GCC/LLVM's 10. See
[Appendix: NCG vs LLVM Code Generation](#appendix-ncg-vs-llvm-code-generation)
for the full assembly analysis.

### 8.6 Task Execution

*Source: [`HsTaskDemo.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsTaskDemo.hs), [`test_omp_tasks.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/test_omp_tasks.c)*

Deferred task execution with work-stealing barriers (4 threads, best of 5):

<!-- BENCH:tasks -->
| Tasks | Sequential | Parallel | Speedup |
|------:|-----------:|---------:|--------:|
| 100   | 1.1 ms     | 0.3 ms   | 3.85x   |
| 500   | 5.7 ms     | 1.5 ms   | 3.93x   |
| 1,000   | 10.8 ms     | 3.6 ms   | 3.03x   |
| 5,000   | 55.5 ms     | 15.5 ms   | 3.58x   |
| 10,000   | 111.1 ms     | 29.4 ms   | 3.77x   |
<!-- /BENCH:tasks -->

Near-linear scaling (3.4-4.0x on 4 threads). Correctness verified against
sequential reference with exact match.

### 8.7 Calling Convention Overhead

*Source: [`HsCmmDemo.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCmmDemo.hs), [`omp_prims.cmm`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_prims.cmm)*

<!-- BENCH:calling_convention -->
| Convention | ns/call | Relative | Mechanism |
|---|---|---|---|
| `foreign import prim` (Cmm) | ~0 | — | Direct register read, GHC optimizes away |
| `foreign import ccall unsafe` | 2.3 | — | Save/restore STG registers |
| `foreign import ccall safe` | 66.6 | 29x vs unsafe | + suspendThread/resumeThread |
<!-- /BENCH:calling_convention -->

<figure>
<img src="charts/ffi-overhead.svg" alt="FFI Calling Convention Overhead" />
</figure>

### 8.8 Batched Calls

*Source: [`HsCmmBatch.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCmmBatch.hs), [`omp_batch.cmm`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_batch.cmm)*

Amortizing the ~68ns safe FFI overhead by batching N C calls within a single
`suspendThread`/`resumeThread` cycle:

<!-- BENCH:batched -->
| Batch size | Standard safe | Cmm batched | Speedup |
|---|---|---|---|
| 1 | 70.7 ns | 68.9 ns | 1.0x |
| 2 | 72.3 ns | 35.9 ns | 2.0x |
| 5 | 71.2 ns | 15.4 ns | 4.6x |
| 10 | 70.7 ns | 8.8 ns | 8.0x |
| 20 | 71.7 ns | 5.3 ns | 13.5x |
| 50 | 71.3 ns | 3.4 ns | 20.9x |
| 100 | 71.1 ns | 2.7 ns | 26.5x |
<!-- /BENCH:batched -->

At batch=100, per-call overhead drops to 2.7 ns — within 35% of unsafe FFI
cost (~2 ns). The results match the theoretical prediction `(68 + N × 2) / N`
closely at every batch size.

<figure>
<img src="charts/batched-calls.svg" alt="Batched Safe Calls: Per-Call Overhead" />
</figure>

### 8.9 Zero-Copy Improvement

*Source: [`HsZeroCopy.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsZeroCopy.hs)*

Haskell sequential DGEMM inner loop, pinned ByteArray with unboxed primops
vs standard boxed FFI:

| N | Boxed (ms) | Unboxed (ms) | Speedup |
|--:|-----------:|-------------:|--------:|
| 256 | 57.3 | 53.4 | 1.07x |
| 512 | 457.9 | 384.6 | **1.19x** |

The 19% improvement at N=512 comes from eliminating `CDouble` boxing in the
O(n³) inner loop. `-ddump-simpl` confirms the hot loop uses `+##`, `*##`, and
`readDoubleArray#` with no `D#` constructor.

---

