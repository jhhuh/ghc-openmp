## 8. Benchmarks

All benchmarks on an Intel i7-10750H (6C/12T), NixOS, GCC 15.2, GHC 9.10.3,
powersave governor. Best-of-N timing to reduce CPU frequency variance.

### 8.1 Microbenchmarks

*Source: [`bench_overhead.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/bench_overhead.c)*

<!-- BENCH:overhead_forkjoin -->
#### Fork/Join Overhead (us/iter)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 0.464 | 0.033 | 14.1x faster |
| 2 | 0.765 | 0.499 | 1.5x faster |
| 4 | 0.931 | 0.945 | 1.02 |
| 8 | 1.461 | 1.692 | 1.16 |
<!-- /BENCH:overhead_forkjoin -->

<!-- BENCH:overhead_barrier -->
#### Barrier Latency (us/iter)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 0.276 | 0.002 | 138.0x faster |
| 2 | 0.242 | 0.137 | 1.8x faster |
| 4 | 0.248 | 0.270 | 1.09 |
| 8 | 0.434 | 0.470 | 1.08 |
<!-- /BENCH:overhead_barrier -->

<!-- BENCH:overhead_parfor -->
#### Parallel For + Reduction (1M sin(), best of 10, ms)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 15.973 | 15.388 | 0.96 |
| 2 | 7.453 | 7.641 | 1.03 |
| 4 | 3.777 | 3.879 | 1.03 |
| 8 | 3.507 | 3.500 | 1.00 |
<!-- /BENCH:overhead_parfor -->

<!-- BENCH:overhead_critical -->
#### Critical Section (1000 lock/unlock per thread, ms)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 0.021 | 0.026 | 1.24 |
| 2 | 0.069 | 0.264 | 3.83 |
| 4 | 0.352 | 0.327 | 1.1x faster |
| 8 | 0.942 | 1.318 | 1.40 |
<!-- /BENCH:overhead_critical -->

### 8.2 DGEMM

*Source: [`bench_dgemm.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/bench_dgemm.c), [`omp_compute.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_compute.c)*

Same naive triple-loop DGEMM compiled identically, linked against either
native libgomp or our runtime. Checksums match exactly.

#### 4 Threads

<!-- BENCH:dgemm_4t -->
| N | Native (ms) | RTS (ms) | Ratio | GFLOPS (RTS) |
|---|---|---|---|---|
| 128 | 0.93 | 1.05 | 1.13x | 4.01 |
| 256 | 10.83 | 13.60 | 1.26x | 2.47 |
| 512 | 78.59 | 83.66 | 1.06x | 3.21 |
| 1024 | 670.05 | 654.66 | 0.98x | 3.28 |
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
| 1 | 2965.94 | 0.72 | 1.0x |
| 2 | 1880.42 | 1.14 | 1.6x |
| 4 | 654.66 | 3.28 | 4.5x |
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
| 100 | 0.7 us | 2.8 us | 0.26x |
| 200 | 3.1 us | 2.9 us | **1.08x** |
| 500 | 8.8 us | 5.8 us | **1.50x** |
| 1000 | 10.2 us | 5.2 us | **1.96x** |
| 5000 | 64.6 us | 21.7 us | **2.98x** |
| 100000 | 1528.7 us | 385.5 us | **3.97x** |
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
| 10K | 327.0 us | 148.0 us | 107.3 us | 42.9 us | 2.50x |
| 100K | 3283.6 us | 1549.8 us | 881.1 us | 375.8 us | 2.34x |
| 1M | 31922.8 us | 15417.0 us | 8637.3 us | 6626.1 us | 1.30x |
| 10M | 336020.5 us | 153998.0 us | 85324.7 us | 40187.1 us | 2.12x |
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
| 100   | 1.5 ms     | 0.4 ms   | 3.81x   |
| 500   | 7.7 ms     | 2.5 ms   | 3.12x   |
| 1,000   | 15.6 ms     | 5.0 ms   | 3.12x   |
| 5,000   | 78.1 ms     | 19.9 ms   | 3.93x   |
| 10,000   | 155.9 ms     | 42.6 ms   | 3.66x   |
<!-- /BENCH:tasks -->

Near-linear scaling (3.4-4.0x on 4 threads). Correctness verified against
sequential reference with exact match.

### 8.7 Calling Convention Overhead

*Source: [`HsCmmDemo.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCmmDemo.hs), [`omp_prims.cmm`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_prims.cmm)*

<!-- BENCH:calling_convention -->
| Convention | ns/call | Relative | Mechanism |
|---|---|---|---|
| `foreign import prim` (Cmm) | ~0 | — | Direct register read, GHC optimizes away |
| `foreign import ccall unsafe` | 3.1 | — | Save/restore STG registers |
| `foreign import ccall safe` | 89.8 | 29x vs unsafe | + suspendThread/resumeThread |
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
| 1 | 98.3 ns | 97.9 ns | 1.0x |
| 2 | 100.0 ns | 50.3 ns | 2.0x |
| 5 | 97.3 ns | 20.8 ns | 4.7x |
| 10 | 97.4 ns | 12.4 ns | 7.8x |
| 20 | 102.6 ns | 7.6 ns | 13.4x |
| 50 | 104.6 ns | 4.7 ns | 22.2x |
| 100 | 97.8 ns | 3.7 ns | 26.4x |
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

