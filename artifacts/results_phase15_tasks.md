# Phase 15: Deferred OpenMP Task Execution

## Summary
Implemented OpenMP task queue with deferred execution and work-stealing
in barriers. Tasks created via `#pragma omp task` are now deferred to a
global queue and stolen by idle threads at barrier points.

## Key Implementation Details

1. **Task Queue**: Global linked list with mutex, atomic pending counter
2. **GOMP_task**: When `if_clause=true` and `tl_in_parallel`, copies data
   to heap (via cpyfn or memcpy) and pushes to queue. Otherwise inline.
3. **Barrier task stealing**: `spin_barrier_wait_tasks` steals tasks while
   spinning. Last thread drains remaining tasks before releasing.
4. **End-of-parallel stealing**: Pool end_barrier uses task-stealing variant,
   since GCC may omit explicit `GOMP_barrier` after `#pragma omp single`.

## Key Discoveries

- GCC passes cpyfn=NULL for simple scalar firstprivate vars (e.g., int).
  It embeds the copy inline, so arg_size includes ALL captured data.
- GCC 15 may omit `GOMP_barrier` after `#pragma omp single` in some
  compilation units. Task stealing must happen at the pool end_barrier.
- arg_size=8 for `run_task_benchmark` (captures both `i` and `work_per_task`).

## Results (4 threads, best of 5)

```
  Tasks    | Sequential   | Parallel     | Speedup
  ---------|--------------|--------------|----------
      100 |      1.7 ms  |      0.3 ms  |  5.41x
      500 |      5.7 ms  |      1.5 ms  |  3.86x
     1000 |     11.3 ms  |      2.9 ms  |  3.88x
     5000 |     57.5 ms  |     14.3 ms  |  4.03x
    10000 |    112.2 ms  |     32.7 ms  |  3.43x
```

## Correctness
- C test: exact match (diff = 0.0e+00) for 10000 tasks x 1000 work/task
- Native libgomp: same result (1952.3080127738)

## vs Native libgomp (C test, 10000 tasks)
- Native: 35.4 ms
- RTS-backed: 61.8 ms (1.7x overhead)
- Overhead from mutex-based queue vs libgomp's lock-free implementation
