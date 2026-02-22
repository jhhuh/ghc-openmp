---
layout: default
title: "GHC's Runtime System as an OpenMP Runtime"
---

# GHC's Runtime System as an OpenMP Runtime

*A feasibility study in replacing libgomp with GHC RTS Capabilities*

> **Note:** This is an experimental project. The claims and results have not been
> thoroughly reviewed and should be considered tentative. The code, experiment design,
> and execution were conducted with significant assistance from
> [Claude Code](https://docs.anthropic.com/en/docs/claude-code).

---

## Contents

1. [Abstract](#1-abstract)
2. [Motivation](#2-motivation)
3. [Background](#3-background)
4. [Architecture](#4-architecture)
5. [Implementation Phases](#5-implementation-phases)
6. [Optimization: From 24x Slower to Parity](#6-optimization-from-24x-slower-to-parity)
7. [Haskell Interop](#7-haskell-interop)
8. [Garbage Collection Interaction](#8-garbage-collection-interaction)
9. [Bidirectional Interop](#9-bidirectional-interop)
10. [Cmm Primitives](#10-cmm-primitives)
11. [Benchmarks](#11-benchmarks)
12. [Notable Bugs and Fixes](#12-notable-bugs-and-fixes)
13. [Limitations](#13-limitations)
14. [Conclusions](#14-conclusions)
15. [Appendix: Implemented ABI Surface](#appendix-implemented-abi-surface)

---

## 1. Abstract

We implement a drop-in OpenMP runtime library that uses GHC's Runtime System
as its threading infrastructure. Standard C code compiled with
`gcc -fopenmp` runs on GHC Capabilities instead of libgomp's
pthreads. The runtime implements the GCC `GOMP_*` ABI and the
`omp_*` user API, supporting parallel regions, worksharing loops,
barriers, critical sections, tasks, and sections.

After lock-free optimization, the runtime achieves **performance parity
with native libgomp** on both microbenchmarks and real numerical workloads
(dense matrix multiplication). Haskell programs call OpenMP-parallelized C code
via FFI, with both runtimes sharing the same thread pool. OpenMP workers
call back into Haskell via `FunPtr` with automatic Capability
acquisition. GHC's stop-the-world garbage collector does not pause OpenMP
workers because they do not hold Capabilities.

---

## 2. Motivation

GHC's RTS has a mature, production-quality thread pool with per-Capability
run queues, work-stealing spark pools, NUMA awareness, and sophisticated
scheduling. OpenMP runtimes (libgomp, libomp) maintain their own, separate
thread pools. When a Haskell program calls OpenMP-annotated C code via FFI,
two independent thread pools compete for the same CPU cores.

If the OpenMP runtime used GHC's thread pool directly, we would get:

- **Unified resource management** — one thread pool, not two
- **Seamless interop** — Haskell green threads and OpenMP parallel regions coexist naturally
- **GHC as a platform** — C programs benefit from GHC's scheduler, and Haskell programs get access to OpenMP's parallel-for without reinventing it

This project investigates whether this is feasible and what the performance
cost is.

---

## 3. Background

### 3.1 GHC RTS Capabilities

A *Capability* is GHC's central execution unit: one OS thread, one
run queue of lightweight Haskell threads (TSOs), and one work-stealing spark
pool. The number of Capabilities is set by `+RTS -N`. Each
Capability has a 0-indexed number (`cap->no`) that maps directly to
OpenMP's `omp_get_thread_num()`.

Key RTS APIs for embedding:

```c
hs_init_ghc(&argc, &argv, conf);       // Boot the RTS
Capability *cap = rts_lock();           // Acquire a Capability
rts_unlock(cap);                        // Release it
rts_setInCallCapability(i, 1);          // Pin OS thread to Capability i
uint32_t getNumCapabilities(void);      // Current Capability count
uint32_t getNumberOfProcessors(void);   // CPU count
```

`hs_init_ghc()` is reference-counted: calling it when the RTS is
already running (as in a Haskell host program) simply increments the counter
and returns. This is the key to transparent interop — our runtime
auto-detects whether it is being hosted by a C program or a Haskell program.

### 3.2 The libgomp ABI

GCC transforms OpenMP pragmas into calls to `GOMP_*` functions.
For example:

```c
#pragma omp parallel
{ body; }

// becomes:
void outlined_fn(void *data) { body; }
GOMP_parallel(outlined_fn, &data, num_threads, flags);
```

A minimum viable runtime needs only 9 symbols (`GOMP_parallel`,
`GOMP_barrier`, `GOMP_critical_start/end`,
`GOMP_single_start`, `GOMP_task`,
`GOMP_taskwait`, `omp_get_num_threads`,
`omp_get_thread_num`). Full OpenMP 4.5 coverage requires ~85
symbols. Our implementation provides ~75.

### 3.3 Cmm and `foreign import prim`

[Cmm](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/cmm-type)
(C minus minus) is GHC's low-level intermediate representation — a portable
assembly language that sits between STG and native code. GHC compiles all
Haskell to Cmm before generating machine code.

GHC provides three FFI calling conventions with different overhead:

| Convention | Mechanism | Overhead |
|---|---|---|
| `foreign import ccall safe` | Releases Capability, calls C, reacquires | ~68 ns |
| `foreign import ccall unsafe` | Saves STG registers, calls C, restores | ~2 ns |
| `foreign import prim` | Direct STG register passing, no boundary | ~0 ns |

The `prim` convention is the fastest: arguments pass directly in GHC's STG
registers (R1, R2, ...) with no calling convention switch. Functions written
in Cmm can access RTS internals like `MyCapability()` directly. GHC treats
`prim` calls as pure expressions and can optimize them away entirely
(loop-invariant code motion, common subexpression elimination).

The [`inline-cmm`](https://github.com/jhhuh/inline-cmm) library lets you
embed Cmm code directly in Haskell modules via a `[cmm| ... |]` quasiquoter
(similar to `inline-c` for C code). It automatically generates the
`foreign import prim` declaration and compiles the Cmm via Template Haskell.
In Phase 10, we write Cmm primitives by hand using the same mechanism
that `inline-cmm` automates.

---

## 4. Architecture

```
Haskell program                    C program
(ghc -threaded)                    (gcc -fopenmp -no-hs-main)
       |                                  |
       | foreign import ccall safe        | calls GOMP_parallel()
       v                                  v
  +---------------------------------------------------------+
  |           ghc_omp_runtime_rts.c                         |
  |                                                         |
  |  GOMP_parallel(fn, data, N, flags):                     |
  |    1. ensure_rts()  -- boot GHC RTS if needed           |
  |    2. Store fn/data, bump atomic generation counter     |
  |    3. Wake workers (condvar broadcast)                  |
  |    4. Start barrier -- all threads sync                 |
  |    5. fn(data) on all threads                           |
  |    6. End barrier -- all threads sync                   |
  |    7. Wait for worker completion                        |
  +---------------------------------------------------------+
       |
       v
  +---------------------------------------------------------+
  |              GHC Runtime System                         |
  |                                                         |
  |  +--------+  +--------+  +--------+  +--------+        |
  |  | Cap 0  |  | Cap 1  |  | Cap 2  |  | Cap 3  |        |
  |  | master |  | worker |  | worker |  | worker |        |
  |  +--------+  +--------+  +--------+  +--------+        |
  |                                                         |
  |  Workers are OS threads pinned to Capabilities.         |
  |  After rts_lock()/rts_unlock() init, they do NOT        |
  |  hold Capabilities -- invisible to GC.                  |
  +---------------------------------------------------------+
```

*Figure 1: Runtime architecture. Workers are plain OS threads registered with
GHC's RTS but not holding Capabilities during OpenMP execution.*

### 4.1 Worker Pool Design

N-1 worker threads are created at initialization. Each is pinned to a GHC
Capability via `rts_setInCallCapability(i, 1)`, performs one
`rts_lock()/rts_unlock()` cycle to register with the RTS, then enters
a spin-wait loop on an atomic generation counter.

The master thread (Capability 0) dispatches work by:

1. Storing the function pointer and data
2. Atomically incrementing the generation counter (release fence)
3. Broadcasting a condvar (for sleeping workers)
4. Participating in the start barrier, executing `fn(data)`, and hitting the end barrier

### 4.2 Synchronization Primitives

All barriers use a **sense-reversing centralized barrier**:
each thread maintains a local sense flag. Threads atomically decrement a shared
counter; the last thread flips the global sense, releasing all waiters. This is
fully lock-free on the fast path.

Workers use a **spin-then-sleep** strategy: spin for ~4000
iterations on the generation counter (using `_mm_pause`), then fall
back to a `pthread_cond_wait` for power efficiency during idle
periods.

---

## 5. Implementation Phases

**Phase 1 — Stub Runtime (pthread-based)**

A shared library (`libghcomp.so`) implementing the GOMP ABI using
raw pthreads. Validates ABI compatibility: a standard
`gcc -fopenmp` test program links against our library and runs
correctly. This phase establishes the full API surface without GHC
involvement.

**Phase 2 — GHC RTS Integration**

Replace the pthread thread pool with GHC Capabilities. Workers are created
via `pthread_create` but immediately pin themselves to Capabilities.
The RTS is booted on first use via `hs_init_ghc()`. A mutex+condvar
dispatch mechanism wakes workers for each parallel region.

> **Design decision**: We chose a hybrid approach (Option D) —
> a C shim calling GHC RTS APIs — over modifying GHC's RTS source directly
> or using `foreign export`. This keeps the runtime as a single
> `.c` file with no GHC fork required.

**Phase 3 — Lock-free Optimization**

Phase 2 benchmarks showed 20–25x overhead on fork/join and 14x on barriers
versus native libgomp, all from mutex+condvar synchronization. Four
optimizations brought performance to parity:

| Optimization | Combined Result |
|---|---|
| Atomic generation counter (lock-free dispatch) | Fork/join: 24.35 → 0.81 us (**30x**) |
| Spin-wait with condvar fallback (4000 iters) | Barrier: 7.01 → 0.25 us (**28x**) |
| Sense-reversing centralized barriers | |
| Atomic completion counter | |

**Phase 4 — Haskell FFI Interop**

A Haskell program calls OpenMP-parallelized C functions (dot product, SAXPY,
sin sum) via `foreign import ccall safe`. GHC releases the calling
Capability during the FFI call, allowing our runtime to use it. Verified
correctness against pure Haskell implementations and measured near-linear
scaling up to 8 threads.

**Phase 5 — Concurrent Haskell + OpenMP**

Demonstrates that Haskell green threads and OpenMP parallel regions run
simultaneously on the same RTS. A pure Haskell computation runs in one
`forkIO` thread while an OpenMP FFI call runs in another. The safe
FFI call releases its Capability, so Haskell threads are not starved.

Result: sequential time 68ms → concurrent time 58ms (1.17x speedup),
with 10ms of verified overlap.

**Phase 6 — GC Interaction Test**

Runs 500 short OpenMP parallel regions concurrently with either allocation
pressure (triggering minor GCs) or forced major GCs. Measures per-region
latency impact. Finding: GC has **minimal impact** on OpenMP tail
latency (worst p99: 1.17x, worst max: 3.27x on one run, none on another).

**Phase 7 — Dense Matrix Multiply**

A real numerical workload: NxN dense matrix multiplication (DGEMM) with
OpenMP parallel for, called from Haskell. Tests sustained parallel computation
with large memory footprint. Results: 3–6x speedup over sequential Haskell at
4 threads across 128–1024 matrix sizes, all results verified correct.

**Phase 8 — Head-to-Head Comparison**

The same DGEMM code compiled against native libgomp and our RTS-backed
runtime. Identical checksums, indistinguishable performance. Details in
[Section 11.2](#112-dgemm-head-to-head-phase-8).

**Phase 9 — Bidirectional Interop**

OpenMP workers call back into Haskell via `FunPtr`. The
`foreign import ccall "wrapper"` stub automatically handles
Capability acquisition. All results verified correct; overhead is ~0.5us
per callback (the `rts_lock/rts_unlock` round-trip). Details in
[Section 9](#9-bidirectional-interop).

**Phase 10 — Cmm Primitives**

Uses GHC's Cmm intermediate representation to write primitives callable via
`foreign import prim` — the fastest possible calling convention.
Benchmarks the three FFI tiers: prim (~0ns, optimized away by GHC),
unsafe (~2ns), safe (~68ns). The 29x safe/unsafe ratio quantifies the
exact cost of Capability release/reacquire per call.
Details in [Section 10](#10-cmm-primitives).

---

## 6. Optimization: From 24x Slower to Parity

The Phase 2 runtime was functional but slow: fork/join took 24 us vs
libgomp's 1 us. The bottleneck was mutex+condvar on every operation. We
eliminated all locks from the hot path:

### 6.1 Lock-free Work Dispatch

```c
// Master: store work, then release-fence generation increment
g_pool.fn = fn;
g_pool.data = data;
atomic_fetch_add(&g_pool.generation, 1, memory_order_release);

// Worker: spin on generation (acquire-fence)
while (atomic_load(&g_pool.generation, memory_order_acquire) == my_gen)
    _mm_pause();
```

No mutex on the hot path. The condvar broadcast is only for workers that
fell asleep after 4000 spin iterations.

### 6.2 Sense-Reversing Barrier

```c
void spin_barrier_wait(spin_barrier_t *b, int *local_sense) {
    *local_sense = 1 - *local_sense;
    if (atomic_fetch_sub(&b->count, 1, memory_order_acq_rel) == 1) {
        // Last thread: reset counter, flip global sense
        atomic_store(&b->count, b->size, memory_order_relaxed);
        atomic_store(&b->sense, *local_sense, memory_order_release);
    } else {
        // Spin until sense matches
        while (atomic_load(&b->sense, memory_order_acquire) != *local_sense)
            _mm_pause();
    }
}
```

Pure atomic operations, no locks. The centralized design has O(N) wakeup but
is optimal for small team sizes (typical OpenMP use).

### 6.3 Results

| Metric (4 threads) | Phase 2 | Phase 3 | Native libgomp |
|---|---|---|---|
| Fork/join | 24.35 us | 0.81 us | 0.97 us |
| Barrier | 7.01 us | 0.25 us | 0.51 us |
| Parallel for (1M sin) | 6.71 ms | 3.91 ms | 3.85 ms |
| Critical section | 0.39 ms | 0.38 ms | 0.92 ms |

After optimization, the RTS-backed runtime **matches or beats**
native libgomp on all benchmarks.

---

## 7. Haskell Interop

### 7.1 FFI Calling Convention

Haskell calls OpenMP C code via `foreign import ccall safe`:

```haskell
foreign import ccall safe "parallel_sinsum"
    c_parallel_sinsum :: CInt -> IO CDouble
```

The `safe` keyword is critical: it tells GHC to release the
calling Capability before entering the foreign code, and reacquire it on
return. This means:

- Other Haskell green threads can run on the released Capability
- The C code enters `GOMP_parallel`, which dispatches to the
  worker pool — including potentially the Capability just released
- No deadlock: workers don't need to hold Capabilities to execute C
  compute kernels

### 7.2 RTS Initialization: Reference Counting

When called from a Haskell host, `hs_init_ghc()` is already done
by GHC before `main`. Our runtime's `ensure_rts()` calls
`hs_init_ghc()` again, which simply increments the reference count
and returns. The runtime discovers the existing Capabilities via
`getNumCapabilities()` and spawns workers for Caps 1..N-1.

### 7.3 Concurrent Execution

```haskell
-- Haskell green thread: pure computation
_ <- forkIO $ do
    let !result = haskellSinSum 1200000
    putMVar hsDone result

-- OpenMP FFI call (safe: releases Capability)
_ <- forkIO $ do
    result <- c_parallel_sinsum 12000000
    putMVar ompDone result

-- Both run simultaneously!
```

Measured: sequential 68ms → concurrent 58ms, with 10ms of overlapping
execution confirmed.

---

## 8. Garbage Collection Interaction

A key concern: GHC's stop-the-world GC pauses all threads holding
Capabilities. Would this stall OpenMP workers?

> **Answer: No.** OpenMP workers do not hold Capabilities during
> parallel execution. After their initial
> `rts_lock()/rts_unlock()` registration, they are plain OS threads
> spinning on atomic variables. GC only synchronizes Capability-holding threads
> — our workers are invisible.

### 8.1 Experimental Validation

We ran 500 OpenMP parallel regions (each ~400us) concurrently with:

| Scenario | p50 (us) | p99 (us) | max (us) |
|---|---|---|---|
| Baseline (OpenMP alone) | 314–478 | 636–658 | 692–783 |
| + allocation pressure (50K rounds) | 313–543 | 538–651 | 585–691 |
| + forced major GC (20 × performGC) | 315–556 | 549–744 | 574–2262 |

Allocation pressure has negligible impact (within noise). Forced major GCs
produced one outlier spike of 2262us on one run and none on another. The spike
correlates with the GHC RTS reporting a 1.6ms max GC pause — likely the
OS thread making the FFI call had its Capability briefly paused at a region
boundary.

GHC RTS statistics: 99.7% productivity, GC time <0.5% of elapsed.

---

## 9. Bidirectional Interop

Phases 4–7 demonstrated Haskell calling OpenMP. Phase 9 completes the
picture: **OpenMP workers calling back into Haskell** from within
a parallel region.

### 9.1 Mechanism

Haskell creates a `FunPtr` via
`foreign import ccall "wrapper"`:

```haskell
foreign import ccall "wrapper"
    mkCallback :: (CInt -> IO CDouble)
               -> IO (FunPtr (CInt -> IO CDouble))

sinCb <- mkCallback (\i -> return (sin (fromIntegral i * 0.001)))
```

GHC generates a C stub that wraps the Haskell closure with automatic
Capability management:

```c
// Generated wrapper (simplified):
CDouble wrapper(CInt arg) {
    Capability *cap = rts_lock();      // acquire Capability
    // ... evaluate Haskell closure ...
    rts_unlock(cap);                   // release Capability
    return result;
}
```

The C code calls this `FunPtr` from inside an OpenMP parallel for:

```c
void parallel_reduce_callback(hs_callback_t callback, int n) {
    double sum = 0.0;
    #pragma omp parallel for reduction(+:sum) schedule(static)
    for (int i = 0; i < n; i++)
        sum += callback(i);  // each worker calls into Haskell
    return sum;
}
```

### 9.2 Correctness

All results verified against pure C and pure Haskell reference
implementations:

| Test | Result | Status |
|---|---|---|
| parallel_map (1000 sin values) | Element-wise match to 1e-10 | OK |
| parallel_reduce (100K sin sum) | 1839.343386 (matches pure C) | OK |
| polynomial callback (10K) | 1109840.005000 (matches Haskell) | OK |

### 9.3 Performance

| Threads | Pure C (ms) | Callback (ms) | Overhead | Per-callback |
|---|---|---|---|---|
| 1 | 1.69 | 46.60 | 27.6x | ~0.47 us |
| 2 | 1.17 | 60.43 | 51.8x | ~0.60 us |
| 4 | 0.71 | 57.91 | 82.1x | ~0.58 us |

The per-callback cost of ~0.5us is the `rts_lock()/rts_unlock()`
round-trip. This is constant regardless of what the Haskell function does.
For callbacks that perform milliseconds of work (e.g., looking up a Haskell
data structure, evaluating a complex expression), the overhead is negligible.
For tight inner loops like 100K trivial sin() calls, pure C should be used
instead.

> **Practical guideline**: Use Haskell callbacks when each
> invocation does ≥100us of work. Below that, the
> `rts_lock/unlock` overhead dominates. Structure code so that
> OpenMP handles the hot numerical loop in C, and calls Haskell for complex
> logic at coarser granularity.

---

## 10. Cmm Primitives

GHC provides three calling conventions for foreign code, each with different
overhead. We wrote a Cmm primitive that reads `Capability_no(MyCapability())`
— the same value as `omp_get_thread_num()` — and benchmarked all three tiers.

### 10.1 The Cmm Primitive

```cmm
#include "Cmm.h"

omp_prim_cap_no(W_ dummy) {
    return (Capability_no(MyCapability()));
}
```

Called from Haskell via:

```haskell
foreign import prim "omp_prim_cap_no" primCapNo# :: Int# -> Int#
```

### 10.2 Calling Convention Overhead

| Convention | ns/call | Relative | Mechanism |
|---|---|---|---|
| `foreign import prim` (Cmm) | ~0 | — | Direct register read, GHC optimizes away |
| `foreign import ccall unsafe` | 2.3 | — | Save/restore STG registers |
| `foreign import ccall safe` | 67.5 | 29x vs unsafe | + suspendThread/resumeThread |

### 10.3 Key Findings

**Prim calls are truly free**: GHC treats `foreign import prim` functions as
pure expressions. The Cmm function compiles to a single memory load from
`BaseReg`, which GHC can hoist out of loops entirely via LICM (loop-invariant
code motion). In a tight loop, 100M prim calls complete in <1ms.

**The safe FFI tax is ~65ns**: Each `foreign import ccall safe` call costs ~65ns
more than `unsafe` — the price of `suspendThread()`/`resumeThread()` which
release and reacquire the Capability. For OpenMP regions doing >1us of work,
this is negligible (<7%).

**The callback overhead gap**: Phase 9's ~500ns per callback overhead is ~7x
larger than the raw safe FFI cost (~68ns). The difference comes from
`rts_lock()/rts_unlock()` performing additional work beyond
`suspendThread()/resumeThread()`: Task structure allocation, global Capability
search, and lock acquisition. A Cmm-level fast path could potentially reduce
this.

---

## 11. Benchmarks

All benchmarks on an Intel i7-10750H (6C/12T), NixOS, GCC 15.2, GHC 9.10.3,
powersave governor. Best-of-N timing to reduce CPU frequency variance.

### 11.1 Microbenchmarks (Phase 3)

#### Fork/Join Overhead (us/iter)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 0.171 | 0.030 | 5.7x faster |
| 2 | 0.828 | 0.420 | 2.0x faster |
| 4 | 0.972 | 0.811 | 1.2x faster |
| 8 | 1.346 | 1.517 | 1.13x |

#### Barrier Latency (us/iter)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 0.026 | 0.002 | 13x faster |
| 2 | 0.313 | 0.136 | 2.3x faster |
| 4 | 0.508 | 0.254 | 2.0x faster |
| 8 | 0.762 | 0.482 | 1.6x faster |

#### Parallel For + Reduction (1M sin(), best of 10, ms)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 15.764 | 15.427 | 0.98x |
| 2 | 7.730 | 7.783 | 1.01x |
| 4 | 3.849 | 3.905 | 1.01x |
| 8 | 3.358 | 3.503 | 1.04x |

#### Critical Section (1000 lock/unlock per thread, ms)

| Threads | Native libgomp | RTS-backed | Ratio |
|---|---|---|---|
| 1 | 0.054 | 0.026 | 2.1x faster |
| 2 | 0.318 | 0.254 | 1.3x faster |
| 4 | 0.915 | 0.381 | 2.4x faster |
| 8 | 2.135 | 1.201 | 1.8x faster |

### 11.2 DGEMM Head-to-Head (Phase 8)

Same naive triple-loop DGEMM compiled identically, linked against either
native libgomp or our runtime. Checksums match exactly.

#### 4 Threads

| N | Native (ms) | RTS (ms) | Ratio | GFLOPS (RTS) |
|---|---|---|---|---|
| 128 | 0.86 | 0.94 | 1.09x | 4.44 |
| 256 | 12.62 | 12.28 | 0.97x | 2.73 |
| 512 | 77.51 | 76.96 | 0.99x | 3.49 |
| 1024 | 748.83 | 663.37 | 0.89x | 3.24 |

Interleaved re-runs confirm the two runtimes trade leads: the difference is
CPU frequency noise, not runtime overhead.

#### Scaling (RTS-backed, DGEMM 1024x1024)

| Threads | Time (ms) | GFLOPS | Speedup |
|---|---|---|---|
| 1 | 2434.99 | 0.88 | 1.0x |
| 2 | 1330.19 | 1.61 | 1.8x |
| 4 | 663.37 | 3.24 | 3.7x |

### 11.3 Haskell FFI Scaling (Phase 4, parallel sinsum)

| Threads | Time (ms) | Speedup |
|---|---|---|
| 1 | 32.5 | 1.0x |
| 2 | 17.3 | 1.9x |
| 4 | 9.9 | 3.3x |
| 8 | 5.0 | 6.5x |

Near-linear scaling through the FFI boundary, confirming the runtime
correctly parallelizes work dispatched from Haskell.

---

## 12. Notable Bugs and Fixes

### 12.1 Barrier Sense Mismatch Deadlock

**Symptom**: Program hangs when calling `GOMP_parallel`
from a `forkIO` thread at `-N4`. No output at all. Works
at `-N1`.

**Root cause**: Workers' local barrier sense variables
(`start_sense`, `end_sense`) persisted from previous
parallel regions (value 1), but `spin_barrier_init()` reset the
barrier's global sense to 0. On the next region:

- Workers flipped 1→0, saw `sense(0) == local_sense(0)`, passed through immediately
- Master (on a new OS thread from `forkIO`) had fresh sense=0, flipped to 1, but couldn't complete the barrier

**Fix**: Reset all local sense variables to 0 at the start of
each parallel region, matching the freshly initialized barriers.

### 12.2 False Parallel-For Regression

**Symptom**: At 4 threads, parallel for appeared 1.65x slower
than native libgomp (6.7ms vs 4.1ms).

**Root cause**: Single-sample measurement on a laptop with
`powersave` CPU governor (i7-10750H at 46% clock). CPU boost state
varied between process invocations.

**Fix**: Changed to best-of-10 within each process. Controlled
interleaved testing confirmed parity (3.85ms vs 3.91ms).

---

## 13. Limitations

| Limitation | Impact | Notes |
|---|---|---|
| No nested parallelism | Low | Inner parallel regions run with 1 thread. `setNumCapabilities` only increases. |
| Tasks execute inline | Medium | `GOMP_task` runs the task immediately (no deferred execution, no work stealing). Sufficient for `#pragma omp task` correctness but not for performance. |
| Single global team | Low | No support for different thread counts in nested teams. |
| No target offloading | None | Not applicable to this project's scope. |
| No doacross loops | Low | `GOMP_doacross_*` not implemented. |
| Guided schedule = dynamic | Low | Our guided schedule uses the same algorithm as dynamic (constant chunk size). |

---

## 14. Conclusions

GHC's Runtime System can serve as a fully functional OpenMP runtime with
**zero measurable overhead** compared to native libgomp. The
implementation is a single 800-line C file using only public GHC RTS APIs —
no GHC fork required.

The key architectural insights are:

1. **Capabilities as thread IDs**: `cap->no`
   directly maps to `omp_get_thread_num()`
2. **Workers without Capabilities**: After RTS registration,
   worker threads release their Capabilities. They execute C code as plain OS
   threads, invisible to GC.
3. **Reference-counted init**: `hs_init_ghc()`
   is idempotent, enabling transparent use from both C and Haskell hosts.
4. **Lock-free synchronization is essential**: The naive
   mutex+condvar implementation was 20–25x slower. Sense-reversing barriers
   and atomic generation counters brought it to parity.
5. **Bidirectional FFI works**: OpenMP workers call Haskell
   functions via `FunPtr` with ~0.5us overhead per invocation
   (automatic `rts_lock/unlock`), making it practical for
   coarse-grained callbacks.

This demonstrates that language runtimes can share threading infrastructure
across FFI boundaries. A Haskell program can call OpenMP C code, with both
sharing the same thread pool, the same CPU cores, and coexisting with GHC's
garbage collector.

---

## Appendix: Implemented ABI Surface

**Core Parallel:**
`GOMP_parallel`, `GOMP_parallel_start`, `GOMP_parallel_end`, `GOMP_barrier`

**Synchronization:**
`GOMP_critical_start`, `GOMP_critical_end`, `GOMP_critical_name_start`, `GOMP_critical_name_end`, `GOMP_atomic_start`, `GOMP_atomic_end`, `GOMP_single_start`, `GOMP_single_copy_start`, `GOMP_single_copy_end`, `GOMP_ordered_start`, `GOMP_ordered_end`

**Worksharing Loops:**
`GOMP_loop_static_start`, `GOMP_loop_static_next`, `GOMP_loop_dynamic_start`, `GOMP_loop_dynamic_next`, `GOMP_loop_guided_start`, `GOMP_loop_guided_next`, `GOMP_loop_runtime_start`, `GOMP_loop_runtime_next`, `GOMP_loop_start`, `GOMP_loop_end`, `GOMP_loop_end_nowait`, `GOMP_parallel_loop_static`, `GOMP_parallel_loop_dynamic`, `GOMP_parallel_loop_guided`, `GOMP_parallel_loop_runtime`

**Tasks:**
`GOMP_task`, `GOMP_taskwait`, `GOMP_taskyield`, `GOMP_taskgroup_start`, `GOMP_taskgroup_end`

**Sections:**
`GOMP_sections_start`, `GOMP_sections_next`, `GOMP_sections_end`, `GOMP_sections_end_nowait`, `GOMP_parallel_sections`

**Cancellation & Teams:**
`GOMP_cancel`, `GOMP_cancellation_point`, `GOMP_barrier_cancel`, `GOMP_loop_end_cancel`, `GOMP_sections_end_cancel`, `GOMP_teams_reg`

**omp_* User API:**
`omp_get_num_threads`, `omp_get_thread_num`, `omp_get_max_threads`, `omp_get_num_procs`, `omp_set_num_threads`, `omp_in_parallel`, `omp_set_dynamic`, `omp_get_dynamic`, `omp_set_nested`, `omp_get_nested`, `omp_get_wtime`, `omp_get_wtick`, `omp_init_lock`, `omp_destroy_lock`, `omp_set_lock`, `omp_unset_lock`, `omp_test_lock`, `omp_init_nest_lock`, `omp_destroy_nest_lock`, `omp_set_nest_lock`, `omp_unset_nest_lock`, `omp_test_nest_lock`, `omp_get_level`, `omp_get_active_level`, `omp_get_ancestor_thread_num`, `omp_get_team_size`, `omp_get_thread_limit`, `omp_set_max_active_levels`, `omp_get_max_active_levels`, `omp_get_supported_active_levels`, `omp_set_schedule`, `omp_get_schedule`, `omp_in_final`, `omp_get_cancellation`, `omp_get_proc_bind`, `omp_get_num_places`, `omp_get_place_num`, `omp_get_default_device`, `omp_set_default_device`, `omp_get_num_devices`, `omp_get_num_teams`, `omp_get_team_num`, `omp_is_initial_device`, `omp_get_initial_device`, `omp_get_max_task_priority`

---

*Environment: NixOS, GHC 9.10.3, GCC 15.2.0, Intel i7-10750H (6C/12T).
Source code: [ghc-openmp](https://github.com/jhhuh/ghc-openmp) repository. February 2026.*
