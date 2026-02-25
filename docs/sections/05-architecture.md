## 4. Architecture

*Source: [`ghc_omp_runtime_rts.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/src/ghc_omp_runtime_rts.c)*

<figure>
<img src="charts/architecture.svg" alt="Runtime architecture diagram" />
</figure>

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

*Source: [`ghc_omp_runtime_rts.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/src/ghc_omp_runtime_rts.c), [`test_omp_tasks.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/src/test_omp_tasks.c)*

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

