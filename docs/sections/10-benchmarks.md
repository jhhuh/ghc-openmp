## Benchmarks {#sec:benchmarks}

<!-- META:system_info -->

### Microbenchmarks {#sec:microbenchmarks}

*Source: [`bench_overhead.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/bench_overhead.c)*

<!-- BENCH:overhead_forkjoin -->
<!-- /BENCH:overhead_forkjoin -->

<!-- BENCH:overhead_barrier -->
<!-- /BENCH:overhead_barrier -->

<!-- BENCH:overhead_parfor -->
<!-- /BENCH:overhead_parfor -->

<!-- BENCH:overhead_critical -->
<!-- /BENCH:overhead_critical -->

### DGEMM {#sec:dgemm}

*Source: [`bench_dgemm.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/bench_dgemm.c), [`omp_compute.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_compute.c)*

Same naive triple-loop DGEMM compiled identically, linked against either
native libgomp or our runtime. Checksums match exactly.

#### 4 Threads

<!-- BENCH:dgemm_4t -->
<!-- /BENCH:dgemm_4t -->

Interleaved re-runs confirm the two runtimes trade leads: the difference is
CPU frequency noise, not runtime overhead.

<figure>
<img src="charts/dgemm-comparison.svg" alt="DGEMM Head-to-Head: Native libgomp vs RTS" />
</figure>

<!-- BENCH:dgemm_scaling -->
<!-- /BENCH:dgemm_scaling -->

### FFI Scaling {#sec:ffi-scaling}

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

### Parallelism Crossover {#sec:parallelism-crossover}

*Source: [`HsCrossover.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCrossover.hs)*

When does OpenMP from Haskell beat sequential C? We measured sinsum
(compute-bound, ~11ns per element) at various sizes with 4 threads.

<!-- BENCH:crossover -->
<!-- /BENCH:crossover -->

The crossover is at **~500 elements** — above this, OpenMP parallel execution
from Haskell is faster than sequential C called via unsafe FFI. The fixed
overhead is ~1.8us (86ns safe FFI + 1712ns OpenMP fork/join).

<figure>
<img src="charts/crossover.svg" alt="Parallelism Crossover: Sequential vs Parallel" />
</figure>

### GHC Native Parallelism vs OpenMP {#sec:par-compare}

*Source: [`HsParCompare.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsParCompare.hs)*

For the same compute-bound sinsum workload, how does Haskell's `forkIO` with
manual work splitting compare to OpenMP via safe FFI?

#### GHC NCG (default code generator)

<!-- BENCH:par_compare -->
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
[Appendix @sec:appendix-ncg-llvm]
for the full assembly analysis.

### Task Execution {#sec:task-execution}

*Source: [`HsTaskDemo.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsTaskDemo.hs), [`test_omp_tasks.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/test_omp_tasks.c)*

Deferred task execution with work-stealing barriers (4 threads, best of 5):

<!-- BENCH:tasks -->
<!-- /BENCH:tasks -->

Near-linear scaling (3.4-4.0x on 4 threads). Correctness verified against
sequential reference with exact match.

### Calling Convention Overhead {#sec:calling-convention-overhead}

*Source: [`HsCmmDemo.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCmmDemo.hs), [`omp_prims.cmm`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_prims.cmm)*

<!-- BENCH:calling_convention -->
<!-- /BENCH:calling_convention -->

<figure>
<img src="charts/ffi-overhead.svg" alt="FFI Calling Convention Overhead" />
</figure>

### Batched Calls {#sec:batched-calls}

*Source: [`HsCmmBatch.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCmmBatch.hs), [`omp_batch.cmm`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_batch.cmm)*

Amortizing the ~68ns safe FFI overhead by batching N C calls within a single
`suspendThread`/`resumeThread` cycle:

<!-- BENCH:batched -->
<!-- /BENCH:batched -->

At batch=100, per-call overhead drops to 2.7 ns — within 35% of unsafe FFI
cost (~2 ns). The results match the theoretical prediction `(68 + N × 2) / N`
closely at every batch size.

<figure>
<img src="charts/batched-calls.svg" alt="Batched Safe Calls: Per-Call Overhead" />
</figure>

---

