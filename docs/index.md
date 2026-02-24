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
5. [Optimization: From 24x Slower to Parity](#5-optimization-from-24x-slower-to-parity)
6. [Haskell Integration](#6-haskell-integration)
7. [Low-Level Techniques](#7-low-level-techniques)
8. [Benchmarks](#8-benchmarks)
9. [Implementation Timeline](#9-implementation-timeline)
10. [Notable Bugs and Fixes](#10-notable-bugs-and-fixes)
11. [Limitations](#11-limitations)
12. [Related Work](#12-related-work)
13. [Conclusions](#13-conclusions)
14. [Appendix: Implemented ABI Surface](#appendix-implemented-abi-surface)

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
symbols. Our implementation provides ~97.

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

---

## 4. Architecture

<pre class="mermaid">
flowchart TD
    subgraph Host["Host Program"]
        direction LR
        HS["Haskell program<br/>(ghc -threaded)"]
        CP["C program<br/>(gcc -fopenmp)"]
    end

    HS -->|"foreign import<br/>ccall safe"| RT
    CP -->|"calls<br/>GOMP_parallel()"| RT

    subgraph RT["ghc_omp_runtime_rts.c"]
        subgraph r1[" "]
            direction LR
            E["1. ensure_rts()"] --> D["2. Store fn/data"] --> W["3. Wake workers"]
        end
        subgraph r2[" "]
            direction LR
            EB["6. End barrier"] ~~~ FN["5. fn(data)"] ~~~ SB["4. Start barrier"]
        end
        W --> SB --> FN --> EB
    end

    RT --> RTS

    subgraph RTS["GHC Runtime System"]
        direction LR
        C0["Cap 0<br/>(master)"] ~~~ C1["Cap 1<br/>(worker)"] ~~~ C2["Cap 2<br/>(worker)"] ~~~ C3["Cap 3<br/>(worker)"]
    end

    style Host fill:#f8fafc,stroke:#94a3b8,stroke-width:2px
    style RT fill:#ecfdf5,stroke:#059669,stroke-width:2px
    style RTS fill:#eff6ff,stroke:#2563eb,stroke-width:2px
    style r1 fill:none,stroke:none
    style r2 fill:none,stroke:none
</pre>

*Figure 1: Runtime architecture. Workers are plain OS threads pinned to
GHC Capabilities. After `rts_lock()`/`rts_unlock()` init, they do NOT
hold Capabilities — invisible to GC.*

> **Design decision**: We chose a hybrid approach — a C shim calling GHC RTS
> APIs — over modifying GHC's RTS source directly or using `foreign export`.
> This keeps the runtime as a single `.c` file with no GHC fork required.

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
iterations (configurable via `OMP_WAIT_POLICY`: passive=100,
active=10000) on the generation counter (using `_mm_pause`), then
`sched_yield()` after the spin threshold, then fall back to a
`pthread_cond_wait` for power efficiency during idle periods.

Worksharing loops support static, dynamic, guided, and runtime scheduling.
**Guided scheduling** uses a CAS-based loop with exponentially-decreasing
chunk sizes (`remaining / nthreads`), giving large initial chunks and
progressively smaller ones for load balancing.

**Serialized nested parallelism**: Inner parallel regions execute with 1
thread. Full `omp_get_level()`, `omp_get_active_level()`,
`omp_get_ancestor_thread_num()`, and `omp_get_team_size()` support with
per-thread nesting state up to 8 levels deep.

### 4.3 Task Queues and Work Stealing

OpenMP tasks (`#pragma omp task`) enable fork-join parallelism where one
thread creates work items and other threads steal them. Our runtime supports
deferred execution: tasks are queued to per-Capability work-stealing queues
and executed by idle threads waiting at barriers.

Each Capability has its own task queue protected by an `atomic_flag` spinlock,
with an atomic pending counter for fast-path bypass:

- **GOMP_task**: When `if_clause` is true and we're in a parallel region,
  copies data to heap (via `cpyfn` or `memcpy`) and pushes to the local
  queue. Otherwise executes inline. Task descriptors are allocated from a
  pre-allocated pool (4096 entries) with per-Capability lock-free free lists.
- **Work stealing**: Idle threads first pop from their own queue, then steal
  from other threads' queues via linear scan from a pseudo-random start.
- **Barrier task stealing**: `spin_barrier_wait_tasks` checks
  `g_tasks_pending` before attempting steals (avoiding expensive atomic
  operations when no tasks exist). The last thread arriving drains remaining
  tasks before releasing the barrier.
- **End-of-parallel stealing**: The pool's end-barrier uses the task-stealing
  variant, since GCC may omit explicit `GOMP_barrier` calls after
  `#pragma omp single`.

Benchmark results are in [Section 8.6](#86-task-execution).

---

## 5. Optimization: From 24x Slower to Parity

The Phase 2 runtime was functional but slow: fork/join took 24 us vs
libgomp's 1 us. The bottleneck was mutex+condvar on every operation. We
eliminated all locks from the hot path:

### 5.1 Lock-free Work Dispatch

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

### 5.2 Sense-Reversing Barrier

The sense-reversing centralized barrier follows Mellor-Crummey & Scott's
algorithm (*"Algorithms for Scalable Synchronization on Shared-Memory
Multiprocessors"*, ACM TOCS 9(1), 1991):

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

### 5.3 Results

| Metric (4 threads) | Phase 2 | Phase 3 | Native libgomp |
|---|---|---|---|
| Fork/join | 24.35 us | 0.81 us | 0.97 us |
| Barrier | 7.01 us | 0.25 us | 0.51 us |
| Parallel for (1M sin) | 6.71 ms | 3.91 ms | 3.85 ms |
| Critical section | 0.39 ms | 0.38 ms | 0.92 ms |

After optimization, the RTS-backed runtime **matches or beats**
native libgomp on all benchmarks.

<pre class="mermaid">
%%{init: {'theme':'neutral','themeVariables':{'xyChart':{'plotColorPalette':'#ef4444,#059669,#64748b'}}}}%%
xychart-beta horizontal
  title "Optimization Journey: Phase 2 → Phase 3 (4 threads, us)"
  x-axis ["Fork/join", "Barrier"]
  y-axis "Latency (us)" 0 --> 25
  bar "Phase 2 (mutex+condvar)" [24.35, 7.01]
  bar "Phase 3 (lock-free)" [0.81, 0.25]
  bar "Native libgomp" [0.97, 0.51]
</pre>

---

## 6. Haskell Integration

This section covers the integration between Haskell and the OpenMP runtime:
calling conventions, initialization, concurrent execution, garbage collection
behavior, and bidirectional callbacks.

### 6.1 FFI Calling Convention

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

### 6.2 RTS Initialization

When called from a Haskell host, `hs_init_ghc()` is already done
by GHC before `main`. Our runtime's `ensure_rts()` calls
`hs_init_ghc()` again, which simply increments the reference count
and returns. The runtime discovers the existing Capabilities via
`getNumCapabilities()` and spawns workers for Caps 1..N-1.

### 6.3 Concurrent Execution

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

### 6.4 Garbage Collection Isolation

A key concern: GHC's stop-the-world GC pauses all threads holding
Capabilities. Would this stall OpenMP workers?

> **Answer: No.** OpenMP workers do not hold Capabilities during
> parallel execution. After their initial
> `rts_lock()/rts_unlock()` registration, they are plain OS threads
> spinning on atomic variables. GC only synchronizes Capability-holding threads
> — our workers are invisible.

#### Experimental Validation

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

### 6.5 Bidirectional Callbacks

The previous sections demonstrated Haskell calling OpenMP. **OpenMP workers
can also call back into Haskell** from within a parallel region.

#### Mechanism

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

#### Correctness

All results verified against pure C and pure Haskell reference
implementations:

| Test | Result | Status |
|---|---|---|
| parallel_map (1000 sin values) | Element-wise match to 1e-10 | OK |
| parallel_reduce (100K sin sum) | 1839.343386 (matches pure C) | OK |
| polynomial callback (10K) | 1109840.005000 (matches Haskell) | OK |

#### Performance

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

## 7. Low-Level Techniques

This section describes advanced techniques for reducing overhead at the
Haskell-C boundary: zero-overhead Cmm primitives, batched FFI calls,
zero-copy data passing, and linear types for safe mutable array partitioning.

### 7.1 Cmm Primitives

GHC provides three calling conventions for foreign code, each with different
overhead. We wrote a Cmm primitive that reads `Capability_no(MyCapability())`
— the same value as `omp_get_thread_num()` — to measure the overhead of each
tier (see [Section 8.7](#87-calling-convention-overhead)).

#### The Cmm Primitive

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

The [`inline-cmm`](https://github.com/jhhuh/inline-cmm) library provides a
`[cmm| ... |]` quasiquoter that eliminates the need for separate `.cmm` files
and manual `foreign import prim` declarations — Template Haskell handles
compilation and linking automatically, producing identical zero-overhead results.

#### Key Findings

**Prim calls are truly free**: GHC treats `foreign import prim` functions as
pure expressions. The Cmm function compiles to a single memory load from
`BaseReg`, which GHC can hoist out of loops entirely via LICM (loop-invariant
code motion). In a tight loop, 100M prim calls complete in <1ms.

**The safe FFI tax is ~65ns**: Each `foreign import ccall safe` call costs ~65ns
more than `unsafe` — the price of `suspendThread()`/`resumeThread()` which
release and reacquire the Capability. For OpenMP regions doing >1us of work,
this is negligible (<7%).

**The callback overhead gap**: The ~500ns per callback overhead from
[Section 6.5](#65-bidirectional-callbacks) is ~7x larger than the raw safe FFI
cost (~68ns). The difference comes from `rts_lock()/rts_unlock()` performing
additional work beyond `suspendThread()/resumeThread()`: Task structure
allocation, global Capability search, and lock acquisition. A Cmm-level fast
path could potentially reduce this.

### 7.2 Batched Safe Calls

The safe FFI tax of ~68ns per call comes from `suspendThread()`/`resumeThread()`,
which release and reacquire the Capability. For workloads that make many short
C calls, this overhead dominates.

By writing the suspend/resume manually in Cmm, we can batch N C calls within
a single Capability release/reacquire cycle, amortizing the ~68ns overhead
across all N calls.

#### The Batching Primitive

```cmm
#include "Cmm.h"

cmm_batched_tid(W_ n) {
    W_ tok; W_ result; W_ new_base; W_ stack;
    W_ i; W_ t;

    /* Save Sp to TSO — GC needs valid stack pointer */
    stack = StgTSO_stackobj(CurrentTSO);
    StgStack_sp(stack) = Sp;

    (tok) = ccall suspendThread(BaseReg "ptr", 0);

    result = 0; i = 0;
    goto loop_check;
loop_body:
    (t) = ccall omp_get_thread_num();
    result = result + t;
    i = i + 1;
loop_check:
    if (i < n) goto loop_body;

    (new_base) = ccall resumeThread(tok);
    BaseReg = new_base;

    /* Restore Sp — GC may have moved the stack */
    stack = StgTSO_stackobj(CurrentTSO);
    Sp = StgStack_sp(stack);

    return (result);
}
```

#### Implementation Details

Three details were critical for correctness:

1. **Save/restore Sp**: `suspendThread` releases the Capability, allowing GC to
   run. The GC needs a valid `Sp` in the TSO to scan the suspended thread's
   stack. Without this, the GC follows a stale stack pointer and crashes.

2. **No `"ptr"` on tok**: The token from `suspendThread` is `void*`, not a
   GC-traceable pointer. Annotating it as `"ptr"` would tell GHC's Cmm compiler
   to treat it as a GC root, causing the GC to follow a non-heap pointer.

3. **State# threading**: `foreign import prim` is pure by default — GHC can
   CSE or hoist the call. Threading `State# RealWorld` through the type
   signature makes GHC treat it as effectful while adding zero runtime cost
   (State# is erased at the Cmm level).

Benchmark results showing speedups up to 27x are in
[Section 8.8](#88-batched-calls).

### 7.3 Zero-Copy FFI with Pinned ByteArray

The standard FFI pattern (`allocaArray` + `peekElemOff`/`pokeElemOff`) boxes
every element as `CDouble` and converts via `realToFrac`, adding overhead at
the Haskell↔OpenMP boundary:

```haskell
-- Boxed: every element goes through CDouble
a <- peekElemOff pA (i * n + k)    -- returns boxed CDouble
b <- peekElemOff pB (k * n + j)    -- returns boxed CDouble
go (acc + realToFrac a * realToFrac b) (k + 1)  -- 4 box/unbox ops
```

Using pinned `ByteArray#` with unboxed primops eliminates this overhead:

```haskell
-- Unboxed: Double# throughout, no boxing
case readDoubleArray# mbaA (i *# n +# k) s of
    (# s', a #) ->     -- a :: Double# (unboxed)
        case readDoubleArray# mbaB (k *# n +# j) s' of
            (# s'', b #) ->     -- b :: Double# (unboxed)
                goK s'' i j (k +# 1#) (acc +## (a *## b))
```

The pinned ByteArray is passed to C via `mutableByteArrayContents#`, which
returns a raw `Addr#` — zero-copy, no marshalling. `touch#` keeps the
ByteArray alive during the C call.

Performance measurements are in [Section 8.9](#89-zero-copy-improvement).

### 7.4 Linear Typed Arrays

GHC's `-XLinearTypes` extension can enforce exclusive ownership
of mutable array regions at compile time. The design is inspired by
[`konn/linear-extra`](https://github.com/konn/linear-extra)'s Borrowable
`SArray`, which uses phantom-tagged tokens for zero-copy split/combine. A
self-contained ~200-line module (`Data.Array.Linear`) extracts the core pattern
and integrates it with the unboxed primops from
[Section 7.3](#73-zero-copy-ffi-with-pinned-bytearray)
(`readDoubleArray#`/`writeDoubleArray#` instead of `Storable`).

The core idea: linearity is on *tokens* (`RW s`), not the array itself. You
need the right token to read/write, and `split`/`combine` tracks disjoint
ownership at the type level.

#### Design

```haskell
-- Linear token: proves exclusive access to region s
data RW s where MkRW :: RW s

-- Array with phantom region (NOT linear — tokens enforce access)
data DArray s = DArray !Int !(MutableByteArray# RealWorld) !Int  -- len, buf, offset

-- Split witness: proves l and r came from splitting s
data SlicesTo s l r where MkSlicesTo :: SlicesTo s l r

-- Operations consume and return tokens
unsafeRead  :: RW s %1 -> DArray s -> Int -> (Double, RW s)
unsafeWrite :: RW s %1 -> DArray s -> Int -> Double -> RW s
split       :: RW s %1 -> Int -> DArray s -> Slice s
combine     :: SlicesTo s l r %1 -> RW l %1 -> RW r %1 -> RW s
```

The `split` operation is zero-copy — both halves share the same underlying
`MutableByteArray#`, just with different offset/length views. No allocation,
no copying, just arithmetic on the offset field.

#### Type-Safe Row-Partitioned DGEMM

```haskell
case split rwC (half * n) arrC of
    MkSlice st rwTop rwBot cTop cBot ->
        let rwTop' = linearDgemm rwTop n 0    half arrA arrB cTop
            rwBot' = linearDgemm rwBot n half half arrA arrB cBot
            rwC'   = combine st rwTop' rwBot'
        in  unsafeRead rwC' arrC 0  -- can only read after recombining
```

This is the Haskell-side type encoding of what OpenMP does at runtime with
`#pragma omp parallel for schedule(static)` — partitioning the output matrix
into disjoint row blocks. The type system statically guarantees:

1. No two computations can write to the same rows
2. The original array cannot be accessed while split
3. All slices must be recombined before reading results

#### C FFI Integration

The `unsafeWithPtr` function passes pinned ByteArray data directly to C:

```haskell
unsafeWithPtr :: DArray s -> (Ptr CDouble -> IO a) -> IO a
```

This threads through IO properly (unlike `runRW#`-based approaches that
create independent state threads), ensuring C side effects are visible to
subsequent Haskell reads.

#### Implementation Notes

- **`unsafeDupablePerformIO` over `runRW#`**: Element access uses
  `unsafeDupablePerformIO` with `{-# NOINLINE #-}` to ensure writes from one
  operation are visible to subsequent reads. `runRW#` creates independent state
  threads that GHC can reorder under `-O2`.
- **Bang patterns for sequencing**: In lazy contexts, token-producing
  operations must be forced with `!` to ensure their side effects execute.
- **Self-contained**: ~200 lines, no dependencies beyond `base` and `ghc-prim`.

---

## 8. Benchmarks

All benchmarks on an Intel i7-10750H (6C/12T), NixOS, GCC 15.2, GHC 9.10.3,
powersave governor. Best-of-N timing to reduce CPU frequency variance.

### 8.1 Microbenchmarks

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

### 8.2 DGEMM

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

<pre class="mermaid">
%%{init: {'theme':'neutral','themeVariables':{'xyChart':{'plotColorPalette':'#64748b,#2563eb'}}}}%%
xychart-beta
  title "DGEMM Head-to-Head: Native libgomp vs RTS (4 threads, ms)"
  x-axis "Matrix size" ["N=128", "N=256", "N=512", "N=1024"]
  y-axis "Time (ms)" 0 --> 800
  bar "Native libgomp" [0.86, 12.62, 77.51, 748.83]
  bar "RTS-backed" [0.94, 12.28, 76.96, 663.37]
</pre>

#### Scaling (RTS-backed, DGEMM 1024x1024)

| Threads | Time (ms) | GFLOPS | Speedup |
|---|---|---|---|
| 1 | 2434.99 | 0.88 | 1.0x |
| 2 | 1330.19 | 1.61 | 1.8x |
| 4 | 663.37 | 3.24 | 3.7x |

### 8.3 FFI Scaling

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

When does OpenMP from Haskell beat sequential C? We measured sinsum
(compute-bound, ~11ns per element) at various sizes with 4 threads.

| Elements | Sequential | Parallel | Speedup |
|---|---|---|---|
| 100 | 0.5 us | 2.1 us | 0.25x |
| 200 | 1.3 us | 2.2 us | 0.56x |
| 500 | 3.6 us | 2.9 us | **1.22x** |
| 1000 | 7.5 us | 3.9 us | 1.94x |
| 5000 | 49.0 us | 16.6 us | 2.95x |
| 100000 | 1132 us | 279 us | 4.06x |

The crossover is at **~500 elements** — above this, OpenMP parallel execution
from Haskell is faster than sequential C called via unsafe FFI. The fixed
overhead is ~1.8us (86ns safe FFI + 1712ns OpenMP fork/join).

<pre class="mermaid">
%%{init: {'theme':'neutral','themeVariables':{'xyChart':{'plotColorPalette':'#d97706,#2563eb'}}}}%%
xychart-beta
  title "Parallelism Crossover: Sequential vs Parallel (4 threads)"
  x-axis "Elements" ["100", "200", "500", "1K", "5K", "100K"]
  y-axis "Time (us)" 0 --> 1200
  line "Sequential C (unsafe FFI)" [0.5, 1.3, 3.6, 7.5, 49.0, 1132]
  line "Parallel OpenMP (safe FFI)" [2.1, 2.2, 2.9, 3.9, 16.6, 279]
</pre>

### 8.5 GHC Native Parallelism vs OpenMP

For the same compute-bound sinsum workload, how does Haskell's `forkIO` with
manual work splitting compare to OpenMP via safe FFI?

| Elements | Seq Haskell | Seq C | Par Haskell | Par OpenMP | Hs/OMP ratio |
|---|---|---|---|---|---|
| 10K | 234 us | 106 us | 78 us | 30 us | 2.6x |
| 100K | 2448 us | 1122 us | 654 us | 287 us | 2.3x |
| 1M | 24290 us | 11425 us | 6203 us | 3269 us | 1.9x |
| 10M | 243418 us | 113917 us | 65448 us | 33303 us | 2.0x |

OpenMP is consistently **~2x faster** than parallel Haskell. The gap comes
entirely from per-element cost (C is 2.1x faster than Haskell for `sin()`
due to SIMD vectorization and no boxing), not from parallelism overhead —
both achieve near-ideal scaling on 4 threads.

### 8.6 Task Execution

Deferred task execution with work-stealing barriers (4 threads, best of 5):

| Tasks | Sequential | Parallel | Speedup |
|------:|-----------:|---------:|--------:|
| 100   | 1.7 ms     | 0.3 ms   | 5.41x   |
| 500   | 5.7 ms     | 1.5 ms   | 3.86x   |
| 1,000 | 11.3 ms    | 2.9 ms   | 3.88x   |
| 5,000 | 57.5 ms    | 14.3 ms  | 4.03x   |
| 10,000| 112.2 ms   | 32.7 ms  | 3.43x   |

Near-linear scaling (3.4-4.0x on 4 threads). Correctness verified against
sequential reference with exact match.

### 8.7 Calling Convention Overhead

| Convention | ns/call | Relative | Mechanism |
|---|---|---|---|
| `foreign import prim` (Cmm) | ~0 | — | Direct register read, GHC optimizes away |
| `foreign import ccall unsafe` | 2.3 | — | Save/restore STG registers |
| `foreign import ccall safe` | 67.5 | 29x vs unsafe | + suspendThread/resumeThread |

<pre class="mermaid">
%%{init: {'theme':'neutral','themeVariables':{'xyChart':{'plotColorPalette':'#2563eb'}}}}%%
xychart-beta
  title "FFI Calling Convention Overhead (ns/call)"
  x-axis ["prim (Cmm)", "ccall unsafe", "ccall safe"]
  y-axis "Latency (ns)" 0 --> 70
  bar [0.1, 2.3, 67.5]
</pre>

### 8.8 Batched Calls

Amortizing the ~68ns safe FFI overhead by batching N C calls within a single
`suspendThread`/`resumeThread` cycle:

| Batch size | Standard safe | Cmm batched | Speedup |
|---|---|---|---|
| 1 | 72.4 ns | 69.1 ns | 1.0x |
| 2 | 71.3 ns | 36.1 ns | 2.0x |
| 5 | 73.0 ns | 15.2 ns | 4.8x |
| 10 | 71.0 ns | 8.7 ns | 8.2x |
| 20 | 71.1 ns | 5.3 ns | 13.5x |
| 50 | 71.7 ns | 3.4 ns | 21.1x |
| 100 | 71.4 ns | 2.7 ns | 26.8x |

At batch=100, per-call overhead drops to 2.7 ns — within 35% of unsafe FFI
cost (~2 ns). The results match the theoretical prediction `(68 + N × 2) / N`
closely at every batch size.

<pre class="mermaid">
%%{init: {'theme':'neutral','themeVariables':{'xyChart':{'plotColorPalette':'#059669,#94a3b8'}}}}%%
xychart-beta
  title "Batched Safe Calls: Per-Call Overhead (ns)"
  x-axis "Batch size" ["1", "2", "5", "10", "20", "50", "100"]
  y-axis "ns/call" 0 --> 75
  line "Cmm batched" [69.1, 36.1, 15.2, 8.7, 5.3, 3.4, 2.7]
  line "Standard safe" [72.4, 71.3, 73.0, 71.0, 71.1, 71.7, 71.4]
</pre>

### 8.9 Zero-Copy Improvement

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
| 14 | GHC native parallelism vs OpenMP — OpenMP ~2x faster | [§8.5](#85-ghc-native-parallelism-vs-openmp) |
| 15 | Deferred task execution with work-stealing barriers | [§4.3](#43-task-queues-and-work-stealing) |
| 16 | Zero-copy FFI with pinned ByteArray — 19% inner loop speedup | [§7.3](#73-zero-copy-ffi-with-pinned-bytearray) |
| 17 | Linear typed arrays for type-safe disjoint partitioning | [§7.4](#74-linear-typed-arrays) |
| 18 | Runtime improvements: guided scheduling, hybrid barriers, task pools | [§4](#4-architecture) |

---

## 10. Notable Bugs and Fixes

### 10.1 Barrier Sense Mismatch Deadlock

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

### 10.2 False Parallel-For Regression

**Symptom**: At 4 threads, parallel for appeared 1.65x slower
than native libgomp (6.7ms vs 4.1ms).

**Root cause**: Single-sample measurement on a laptop with
`powersave` CPU governor (i7-10750H at 46% clock). CPU boost state
varied between process invocations.

**Fix**: Changed to best-of-10 within each process. Controlled
interleaved testing confirmed parity (3.85ms vs 3.91ms).

---

## 11. Limitations

| Limitation | Impact | Notes |
|---|---|---|
| Serialized nesting only | Low | Inner parallel regions execute with 1 thread. True nested parallelism (multiple active levels) is not supported. |
| Single global team | Low | No support for different thread counts in nested teams. |
| No target offloading | None | Not applicable to this project's scope. |
| No doacross loops | Low | `GOMP_doacross_*` not implemented. |

---

## 12. Related Work

**BOLT** ([bolt-omp.org](https://www.bolt-omp.org/), Best Paper PACT '19) is
the closest analogue to this project. BOLT is a full OpenMP runtime built on
[Argobots](https://www.argobots.org/), a lightweight user-level threading
library from Argonne National Laboratory. Where libgomp maps OpenMP threads
to pthreads, BOLT maps them to Argobots *user-level threads* (ULTs) scheduled
on *execution streams* (ES) — achieving efficient nested parallelism and
fine-grained tasking that pthreads cannot.

The architectural parallel is direct:

| Concept | BOLT / Argobots | ghc-openmp / GHC RTS |
|---------|----------------|---------------------|
| OS-thread abstraction | Execution Stream (ES) | Capability |
| Lightweight work unit | ULT / Tasklet | Haskell green thread |
| OpenMP thread mapping | ULT on ES | OS thread pinned to Capability |
| Scheduler | Pluggable per-pool | GHC spark pool + spin-wait workers |
| Work stealing | Built-in | Phase 15 deferred tasks |

The key difference is motivation: BOLT starts from a *purpose-built* threading
substrate (Argobots) designed for composing HPC runtimes (MPI + OpenMP +
task libraries). ghc-openmp repurposes an *existing language runtime* that
already provides green threads, garbage collection, and an FFI — trading
Argobots' generality for seamless Haskell interoperation.

---

## 13. Conclusions

GHC's Runtime System can serve as a fully functional OpenMP runtime with
**zero measurable overhead** compared to native libgomp. The
implementation is a single ~1300-line C file using only public GHC RTS APIs —
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
`GOMP_loop_static_start`, `GOMP_loop_static_next`, `GOMP_loop_dynamic_start`, `GOMP_loop_dynamic_next`, `GOMP_loop_guided_start`, `GOMP_loop_guided_next`, `GOMP_loop_runtime_start`, `GOMP_loop_runtime_next`, `GOMP_loop_start`, `GOMP_loop_end`, `GOMP_loop_end_nowait`, `GOMP_loop_nonmonotonic_dynamic_start`, `GOMP_loop_nonmonotonic_dynamic_next`, `GOMP_loop_nonmonotonic_guided_start`, `GOMP_loop_nonmonotonic_guided_next`, `GOMP_parallel_loop_static`, `GOMP_parallel_loop_dynamic`, `GOMP_parallel_loop_guided`, `GOMP_parallel_loop_runtime`, `GOMP_parallel_loop_nonmonotonic_dynamic`, `GOMP_parallel_loop_nonmonotonic_guided`

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
