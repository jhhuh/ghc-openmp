# Decision: Implementation Approach for GHC-backed OpenMP Runtime

## Context

We have:
1. A thorough understanding of GHC RTS internals (Capabilities, Sparks, Tasks, WSDeque)
2. A complete catalog of the libgomp ABI (~85 symbols for full OpenMP 4.5)
3. A working pthread-based stub `libghcomp.so` that passes all basic tests

The question: **How do we replace pthreads with GHC's RTS?**

## Options Evaluated

### Option A: C shim that calls GHC RTS C API
Replace pthreads in `ghc_omp_runtime.c` with calls to GHC's RTS C functions:
- `hs_init_ghc()` to boot the RTS
- `rts_lock()`/`rts_unlock()` per worker to acquire Capabilities
- `rts_setInCallCapability(i, 1)` to pin workers to specific caps
- Use `cap->no` as `omp_get_thread_num()`

**Pros**: Minimal Haskell knowledge needed. Uses stable public API. Single `.c` file.
**Cons**: Still spawns our own OS threads (just pins them to Capabilities). Doesn't
actually use GHC's scheduler/spark pool for work dispatch. Essentially "pthreads with
extra steps" — we get Capability IDs and affinity but not the scheduler.

### Option B: Haskell `foreign export` bridge
Write a Haskell library that exports the GOMP_* symbols via `foreign export ccall`.
The Haskell code uses `forkIO` / `Control.Concurrent` to implement parallelism.

**Pros**: Full access to GHC's scheduler, sparks, STM. Can use `par`/`pseq` for tasks.
**Cons**: Every OpenMP call crosses the FFI boundary. Can't easily map `forkIO` threads
to deterministic thread IDs. `forkIO` creates lightweight threads, not Capability-bound
workers. Major complexity in `foreign export` ABI matching.

### Option C: Modify GHC RTS source directly
Fork GHC's RTS, add OpenMP entry points (`GOMP_*`) directly in C alongside the
existing scheduler code. Workers are Capabilities; sparks become tasks.

**Pros**: Maximum performance. Direct access to all internal state. Can add a "C task
queue" per Capability (parallel to the spark pool). No FFI overhead.
**Cons**: Requires maintaining a GHC fork. Tight coupling to GHC version. Highest
development effort. Harder to upstream.

### Option D (Hybrid): C shim + GHC RTS as thread pool ← **RECOMMENDED**
Keep our `ghc_omp_runtime.c` as the entry point, but:
1. Initialize the GHC RTS on first `GOMP_parallel` call
2. Use **one OS thread per Capability** as the worker pool (GHC already creates these)
3. Dispatch work to Capabilities via `rts_lock()`/`rts_setInCallCapability()` or via
   the message inbox (`sendMessage` to a Capability)
4. Use the WSDeque directly (it's a pure C data structure) for task work-stealing
5. Build barriers using GHC's `Condition`/`Mutex` from OSThreads.h

**Key insight**: GHC's RTS already has N OS threads (one per Capability). We don't
need to create our own — we need to **submit work items** to the existing threads.

## Decision: **Option D (Hybrid)**

### Architecture

```
┌────────────────────────────────────────────────────────┐
│  C program with #pragma omp parallel                   │
│  compiled with gcc -fopenmp                            │
└────────────┬───────────────────────────────────────────┘
             │ calls GOMP_parallel(fn, data, N, flags)
             ▼
┌────────────────────────────────────────────────────────┐
│  libghcomp.so  (our C shim)                            │
│  - Translates GOMP_* ABI to GHC RTS calls              │
│  - Manages thread-local OpenMP state                   │
│  - Implements barriers, worksharing, reductions        │
└────────────┬───────────────────────────────────────────┘
             │ calls rts_lock / rts_eval / messages
             ▼
┌────────────────────────────────────────────────────────┐
│  GHC RTS (linked as a library)                         │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐     │
│  │ Cap 0   │ │ Cap 1   │ │ Cap 2   │ │ Cap N-1 │     │
│  │ (OS th) │ │ (OS th) │ │ (OS th) │ │ (OS th) │     │
│  │ WSDeque  │ │ WSDeque  │ │ WSDeque  │ │ WSDeque  │     │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘     │
│  Work-stealing scheduler                               │
└────────────────────────────────────────────────────────┘
```

### Phase 2 Implementation Plan

1. **Boot GHC RTS**: Call `hs_init_ghc()` on first GOMP_parallel, with `-N<threads>` to
   match `OMP_NUM_THREADS`. This creates the Capability array and worker OS threads.

2. **GOMP_parallel**: Instead of `pthread_create`, submit `fn(data)` to each Capability
   by calling `rts_lock()` from N threads pinned to Capabilities via
   `rts_setInCallCapability()`. Each thread gets `cap->no` as its thread ID.

3. **Worker pool**: Maintain a pool of N OS threads (created once, reused across
   parallel regions). Each OS thread is pinned to a Capability.

4. **GOMP_barrier**: Keep our existing mutex+condvar barrier — GHC's OS-level
   primitives (Mutex, Condition from OSThreads.h) are equivalent.

5. **GOMP_task**: Push work items to a per-Capability WSDeque (we can allocate our own
   WSDeques using the same code GHC uses for sparks). Workers steal from each other.

6. **Worksharing**: Same static/dynamic partitioning, just using `cap->no` for thread ID.

### Why this works

- GHC's RTS is designed to be embedded in C programs (`hs_init_ghc`)
- `rts_lock()`/`rts_unlock()` is the official API for C→Haskell calls
- `rts_setInCallCapability()` gives us deterministic Capability→OS thread mapping
- The WSDeque is a standalone C data structure we can reuse or re-create
- We keep full control over the GOMP_* ABI in our C shim
- If we later want Haskell interop (e.g., calling Haskell from OpenMP regions),
  we already have the RTS running

### Open questions for Phase 2
- Can `rts_lock()` from N simultaneous threads acquire N distinct Capabilities?
- What's the overhead of `rts_lock()`/`rts_unlock()` vs raw pthread operations?
- Does the GC interfere with OpenMP compute loops? (Need benchmarks)
- Should we use GHC's WSDeque for GOMP_task, or keep our atomic-counter approach?
