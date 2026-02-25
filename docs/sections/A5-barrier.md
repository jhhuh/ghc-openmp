## Appendix: Sense-Reversing Barrier

*Source: [`spin_barrier_wait`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/src/ghc_omp_runtime_rts.c#FN:spin_barrier_wait), [`spin_barrier_wait_tasks`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/src/ghc_omp_runtime_rts.c#FN:spin_barrier_wait_tasks)*

The runtime's barrier is a **centralized sense-reversing barrier** from
Mellor-Crummey & Scott ("Algorithms for Scalable Synchronization on
Shared-Memory Multiprocessors", ACM TOCS 9(1), 1991).

### Data Structure

```c
typedef struct {
    atomic_int count;   /* threads remaining (decremented by arrivals) */
    atomic_int sense;   /* global sense flag (flipped by last arrival) */
    int size;           /* team size (reset value for count) */
} spin_barrier_t;
```

Each thread also maintains a thread-local `int local_sense`, initially 0.

### Algorithm

1. **Arrive**: Thread flips its `local_sense` (`1 - local_sense`), then
   atomically decrements `count` with `memory_order_acq_rel`.

2. **Last thread** (decrement returns 1): Resets `count` to `size`
   (relaxed store), then flips the global `sense` to match `local_sense`
   (release store). This releases all waiting threads.

3. **Other threads**: Spin-wait until the global `sense` matches their
   `local_sense` (acquire load).

The sense-reversing trick avoids the resetting race in naive barriers: because
each phase uses the opposite sense value, threads that haven't yet left a
barrier cannot be confused with threads entering the next one.

### Hybrid Spin-Wait

Spinning uses a three-tier strategy controlled by `g_spin_iters`
(configurable via `OMP_WAIT_POLICY`):

1. **Spin with `pause`**: For the first `g_spin_iters` iterations (~4000
   default), execute `_mm_pause()` to reduce pipeline contention and save
   power on x86.
2. **`sched_yield()`**: After the spin threshold, yield the CPU to other
   threads. This avoids wasting cycles during longer waits.
3. **Condvar fallback**: The worker pool's generation-counter wait (not the
   barrier itself) uses `pthread_cond_wait` for idle periods between
   parallel regions.

The `OMP_WAIT_POLICY` environment variable controls aggressiveness:
`active` sets 10000 spin iterations, `passive` sets 100.

### Task-Stealing Variant

[`spin_barrier_wait_tasks`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/src/ghc_omp_runtime_rts.c#FN:spin_barrier_wait_tasks)
extends the barrier with work stealing during the spin-wait phase:

- **While waiting**: If `g_tasks_pending > 0`, attempt to steal and
  execute a task from another thread's queue. Reset the spin counter
  after productive work.
- **Last thread**: Before releasing the barrier, drain all remaining
  tasks (spin on `g_tasks_pending` until zero). This ensures no tasks
  are lost when GCC omits explicit `GOMP_barrier` calls after
  `#pragma omp single`.

The `g_tasks_pending` fast-path check (acquire load of a single atomic)
avoids the cost of scanning per-thread queues when no tasks exist. This
is critical for the common case where barriers are used without tasks.
