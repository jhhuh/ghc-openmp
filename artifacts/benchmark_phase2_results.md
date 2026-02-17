# Benchmark: Phase 2 GHC RTS-backed Runtime vs Native libgomp

## Setup
- **CPU**: Host machine (see `nproc`)
- **GCC**: 15.2.0, `-O2 -fopenmp`
- **GHC**: 9.10.3, threaded RTS
- **Native**: libgomp (GCC bundled)
- **RTS-backed**: `ghc_omp_runtime_rts.c` linked via `ghc -threaded -no-hs-main`
- **Benchmark**: `bench_overhead.c` — 5 microbenchmarks, 10000 iterations each

## Results

### Fork/Join Overhead (empty parallel region, us/iter)

| Threads | Native | RTS-backed | Ratio |
|---------|--------|------------|-------|
| 1       |  0.248 |      0.030 | 0.12x (RTS faster — single-thread fast path) |
| 2       |  0.845 |     16.846 |  20x  |
| 4       |  1.020 |     24.354 |  24x  |
| 8       |  1.470 |     34.313 |  23x  |

**Analysis**: The single-thread fast path (direct `fn(data)` call, no pool) beats
native libgomp. Multi-thread overhead comes from mutex lock + generation increment +
broadcast + barrier_wait per region. libgomp uses optimized futex-based spinning.

### Barrier Latency (us/iter)

| Threads | Native | RTS-backed | Ratio |
|---------|--------|------------|-------|
| 1       |  0.040 |      0.002 | 0.05x |
| 2       |  0.300 |      3.954 |  13x  |
| 4       |  0.491 |      7.013 |  14x  |
| 8       |  0.818 |     13.402 |  16x  |

**Analysis**: Our barrier uses a global mutex + condvar (centralized). libgomp uses
a tree-based dissemination barrier with spinning. Room for improvement with lock-free
or tree barriers.

### Parallel For + Reduction (1M sin() iterations, ms)

| Threads | Native | RTS-backed | Ratio |
|---------|--------|------------|-------|
| 1       | 26.727 |     16.976 | 0.64x (RTS faster) |
| 2       |  8.318 |      7.781 | 0.94x |
| 4       |  4.160 |      6.707 | 1.61x |
| 8       |  3.431 |      3.481 | 1.01x |

**Analysis**: For compute-bound workloads, performance is comparable. The overhead
per fork/join is amortized when there's significant work per iteration. At 8 threads,
the two runtimes are essentially identical.

### Critical Section (1000 lock/unlock per thread, ms)

| Threads | Native | RTS-backed | Ratio |
|---------|--------|------------|-------|
| 1       |  0.058 |      0.026 | 0.45x |
| 2       |  0.326 |      0.272 | 0.83x |
| 4       |  0.933 |      0.392 | 0.42x |
| 8       |  1.976 |      1.184 | 0.60x |

**Analysis**: RTS-backed runtime is consistently faster for critical sections.
Both use `pthread_mutex_lock/unlock` underneath, but libgomp adds bookkeeping
overhead (team state, nesting tracking). Our implementation is a bare mutex.

### Task Creation (10000 tasks, ms)

| Threads | Native | RTS-backed | Ratio |
|---------|--------|------------|-------|
| 1       |  0.903 |      0.032 |  28x faster |
| 2       |  1.914 |      0.041 |  47x faster |
| 4       |  5.008 |      0.046 | 109x faster |
| 8       |  6.384 |      0.076 |  84x faster |

**Note**: This comparison is **unfair**. Our `GOMP_task` executes tasks inline
(synchronous, no queuing). libgomp creates real deferred tasks with work-stealing
queues. Our number reflects "function call overhead" while native reflects actual
task scheduling infrastructure. A fair comparison requires implementing deferred
task execution.

## Summary

| Category | Verdict |
|----------|---------|
| Fork/join overhead | ~20-25x slower (mutex+condvar vs futex spinning) |
| Barrier latency | ~14x slower (centralized vs tree-based) |
| Compute-bound work | Comparable (1-1.6x, amortized overhead) |
| Critical sections | 1.5-2.4x faster (bare mutex vs libgomp bookkeeping) |
| Tasks | Unfair comparison (inline vs deferred) |

## Key Takeaway

**For compute-heavy parallel regions, the RTS-backed runtime is viable.** The
overhead is concentrated in fork/join and barrier synchronization — exactly
the areas where libgomp has highly optimized, architecture-specific
implementations (futex, spinning, tree barriers). These are known optimization
targets for Phase 3.

## Phase 3 Optimization Opportunities

1. **Spinning before blocking**: Add brief spin-wait loops before falling through
   to condvar wait (hybrid spinning approach)
2. **Tree-based barriers**: Replace centralized barrier with tournament/dissemination
   barrier for O(log N) latency
3. **Lock-free generation broadcast**: Use atomic generation counter with futex_wait
   instead of mutex+condvar
4. **Deferred task execution**: Implement per-capability task queues (possibly using
   GHC's WSDeque) for actual work-stealing tasks
5. **Leverage GHC scheduler**: Instead of our own pthread barriers, submit work items
   as Haskell sparks or stable pointers evaluated by the RTS scheduler
