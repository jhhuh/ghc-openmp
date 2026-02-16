# libgomp ABI Catalog (GCC 11.2 / 15.2)

## Source: `nm -D libgomp.so` + test program compilation

---

## Minimum Viable Symbols

These 9 symbols are all that `gcc -fopenmp` links against for a program using
`#pragma omp parallel`, `parallel for reduction`, `critical`, `single`,
`barrier`, `task`, and `taskwait`:

```
GOMP_barrier
GOMP_critical_end
GOMP_critical_start
GOMP_parallel
GOMP_single_start
GOMP_task
GOMP_taskwait
omp_get_num_threads
omp_get_thread_num
```

---

## Full ABI by Category

### Tier 1: Core Parallel (GOMP_1.0) — Implement First

| Symbol | Signature (from GCC source) | Purpose |
|---|---|---|
| `GOMP_parallel` | `void GOMP_parallel(void (*fn)(void *), void *data, unsigned num_threads, unsigned flags)` | Fork a team, run fn on each thread, join |
| `GOMP_parallel_start` | `void GOMP_parallel_start(void (*fn)(void *), void *data, unsigned num_threads)` | Legacy: fork team |
| `GOMP_parallel_end` | `void GOMP_parallel_end(void)` | Legacy: join team |
| `GOMP_barrier` | `void GOMP_barrier(void)` | Barrier synchronization |
| `GOMP_critical_start` | `void GOMP_critical_start(void)` | Enter default critical section |
| `GOMP_critical_end` | `void GOMP_critical_end(void)` | Leave default critical section |
| `GOMP_critical_name_start` | `void GOMP_critical_name_start(void **pptr)` | Enter named critical section |
| `GOMP_critical_name_end` | `void GOMP_critical_name_end(void **pptr)` | Leave named critical section |
| `GOMP_atomic_start` | `void GOMP_atomic_start(void)` | Fallback mutex for non-atomic-capable ops |
| `GOMP_atomic_end` | `void GOMP_atomic_end(void)` | End fallback mutex |
| `GOMP_single_start` | `bool GOMP_single_start(void)` | Returns true for exactly one thread |
| `GOMP_single_copy_start` | `void *GOMP_single_copy_start(void)` | Single + copyprivate: start |
| `GOMP_single_copy_end` | `void GOMP_single_copy_end(void *data)` | Single + copyprivate: end |
| `GOMP_ordered_start` | `void GOMP_ordered_start(void)` | Ordered region start |
| `GOMP_ordered_end` | `void GOMP_ordered_end(void)` | Ordered region end |

### Tier 1: Worksharing Loops (GOMP_1.0)

| Symbol | Purpose |
|---|---|
| `GOMP_loop_static_start` | Static schedule: get first chunk |
| `GOMP_loop_static_next` | Static schedule: get next chunk |
| `GOMP_loop_dynamic_start` | Dynamic schedule: get first chunk |
| `GOMP_loop_dynamic_next` | Dynamic schedule: get next chunk |
| `GOMP_loop_guided_start` | Guided schedule: get first chunk |
| `GOMP_loop_guided_next` | Guided schedule: get next chunk |
| `GOMP_loop_runtime_start` | Runtime-determined schedule: start |
| `GOMP_loop_runtime_next` | Runtime-determined schedule: next |
| `GOMP_loop_end` | End worksharing loop (implicit barrier) |
| `GOMP_loop_end_nowait` | End worksharing loop (no barrier) |

Signatures follow pattern:
```c
bool GOMP_loop_static_start(long start, long end, long incr, long chunk_size,
                            long *istart, long *iend);
bool GOMP_loop_static_next(long *istart, long *iend);
```

### Tier 1: User-Facing omp_* API (OMP_1.0 / OMP_3.0)

| Symbol | Signature | Purpose |
|---|---|---|
| `omp_get_num_threads` | `int omp_get_num_threads(void)` | Threads in current team |
| `omp_get_thread_num` | `int omp_get_thread_num(void)` | Current thread ID (0-indexed) |
| `omp_get_max_threads` | `int omp_get_max_threads(void)` | Max threads available |
| `omp_get_num_procs` | `int omp_get_num_procs(void)` | Number of processors |
| `omp_set_num_threads` | `void omp_set_num_threads(int)` | Set default thread count |
| `omp_in_parallel` | `int omp_in_parallel(void)` | Currently in parallel region? |
| `omp_set_dynamic` | `void omp_set_dynamic(int)` | Enable dynamic thread count |
| `omp_get_dynamic` | `int omp_get_dynamic(void)` | Dynamic thread count enabled? |
| `omp_get_wtime` | `double omp_get_wtime(void)` | Wall clock time |
| `omp_get_wtick` | `double omp_get_wtick(void)` | Timer precision |
| `omp_init_lock` | `void omp_init_lock(omp_lock_t *)` | Initialize lock |
| `omp_destroy_lock` | `void omp_destroy_lock(omp_lock_t *)` | Destroy lock |
| `omp_set_lock` | `void omp_set_lock(omp_lock_t *)` | Acquire lock |
| `omp_unset_lock` | `void omp_unset_lock(omp_lock_t *)` | Release lock |
| `omp_test_lock` | `int omp_test_lock(omp_lock_t *)` | Try acquire lock |
| `omp_init_nest_lock` | `void omp_init_nest_lock(omp_nest_lock_t *)` | Reentrant lock init |
| `omp_destroy_nest_lock` | | Reentrant lock destroy |
| `omp_set_nest_lock` | | Reentrant lock acquire |
| `omp_unset_nest_lock` | | Reentrant lock release |
| `omp_test_nest_lock` | | Reentrant lock try |
| `omp_set_nested` | `void omp_set_nested(int)` | Enable nested parallelism |
| `omp_get_nested` | `int omp_get_nested(void)` | Nested parallelism enabled? |

### Tier 2: Tasks (GOMP_2.0 / GOMP_4.0)

| Symbol | Signature | Purpose |
|---|---|---|
| `GOMP_task` | `void GOMP_task(void (*fn)(void *), void *data, void (*cpyfn)(void *, void *), long arg_size, long arg_align, bool if_clause, unsigned flags, void **depend, int priority)` | Create a task |
| `GOMP_taskwait` | `void GOMP_taskwait(void)` | Wait for child tasks |
| `GOMP_taskyield` | `void GOMP_taskyield(void)` | Yield current task |
| `GOMP_taskgroup_start` | `void GOMP_taskgroup_start(void)` | Begin taskgroup |
| `GOMP_taskgroup_end` | `void GOMP_taskgroup_end(void)` | End taskgroup (implicit wait) |
| `GOMP_taskloop` | (complex) | Create a task loop |

### Tier 2: Sections (GOMP_1.0)

| Symbol | Purpose |
|---|---|
| `GOMP_sections_start` | Start sections construct |
| `GOMP_sections_next` | Get next section number |
| `GOMP_sections_end` | End sections (implicit barrier) |
| `GOMP_sections_end_nowait` | End sections (no barrier) |
| `GOMP_parallel_sections_start` | Combined parallel sections |
| `GOMP_parallel_sections` | Combined parallel sections (4.0) |

### Tier 2: Combined Parallel Loops (GOMP_1.0 / GOMP_4.0)

| Symbol | Purpose |
|---|---|
| `GOMP_parallel_loop_static_start` | Legacy: parallel + static loop |
| `GOMP_parallel_loop_dynamic_start` | Legacy: parallel + dynamic loop |
| `GOMP_parallel_loop_guided_start` | Legacy: parallel + guided loop |
| `GOMP_parallel_loop_runtime_start` | Legacy: parallel + runtime loop |
| `GOMP_parallel_loop_static` | 4.0: parallel + static loop |
| `GOMP_parallel_loop_dynamic` | 4.0: parallel + dynamic loop |
| `GOMP_parallel_loop_guided` | 4.0: parallel + guided loop |
| `GOMP_parallel_loop_runtime` | 4.0: parallel + runtime loop |

### Tier 3: OpenMP 3.0+ Extensions

| Symbol | Version | Purpose |
|---|---|---|
| `omp_get_level` | OMP_3.0 | Nesting level |
| `omp_get_active_level` | OMP_3.0 | Active nesting level |
| `omp_get_ancestor_thread_num` | OMP_3.0 | Thread num at given level |
| `omp_get_team_size` | OMP_3.0 | Team size at given level |
| `omp_get_thread_limit` | OMP_3.0 | Thread limit |
| `omp_set_max_active_levels` | OMP_3.0 | Set max nesting |
| `omp_get_max_active_levels` | OMP_3.0 | Get max nesting |
| `omp_get_schedule` | OMP_3.0 | Get default schedule |
| `omp_set_schedule` | OMP_3.0 | Set default schedule |
| `omp_in_final` | OMP_3.1 | In final task? |

### Tier 3: OpenMP 4.0+ (Cancel, Doacross, Target)

| Symbol | Purpose |
|---|---|
| `GOMP_cancel` | Cancellation |
| `GOMP_cancellation_point` | Cancellation check point |
| `GOMP_barrier_cancel` | Barrier with cancellation |
| `GOMP_loop_end_cancel` | Loop end with cancellation |
| `GOMP_sections_end_cancel` | Sections end with cancellation |
| `GOMP_doacross_post` | Doacross: signal iteration |
| `GOMP_doacross_wait` | Doacross: wait for iteration |
| `GOMP_teams` | Teams construct |
| `GOMP_target` | Target offloading |
| `GOMP_target_data` | Target data mapping |
| `GOMP_target_update` | Target data update |
| `GOMP_target_end_data` | End target data |
| `omp_get_cancellation` | OMP_4.0 |
| `omp_get_proc_bind` | OMP_4.0 |
| `omp_get_num_teams` | OMP_4.0 |
| `omp_get_team_num` | OMP_4.0 |
| `omp_get_default_device` | OMP_4.0 |
| `omp_set_default_device` | OMP_4.0 |
| `omp_get_num_devices` | OMP_4.0 |
| `omp_is_initial_device` | OMP_4.0 |

### Tier 4: OpenMP 5.0+ (Allocators, Affinity, Reductions)

| Symbol | Purpose |
|---|---|
| `GOMP_parallel_reductions` | Parallel with reductions |
| `GOMP_taskgroup_reduction_register` | Task reduction registration |
| `GOMP_taskgroup_reduction_unregister` | Task reduction cleanup |
| `GOMP_task_reduction_remap` | Task reduction remapping |
| `GOMP_workshare_task_reduction_unregister` | Workshare reduction cleanup |
| `GOMP_taskwait_depend` | Taskwait with dependencies |
| `GOMP_alloc` / `GOMP_free` | OpenMP allocators |
| `omp_init_allocator` / `omp_destroy_allocator` | Allocator management |
| `omp_alloc` / `omp_free` | Allocator-aware allocation |
| `omp_set/get_affinity_format` | Thread affinity display |
| `omp_display_affinity` | Show affinity |
| `omp_capture_affinity` | Capture affinity to string |
| `omp_fulfill_event` | Detachable tasks |

### Not in scope: OpenACC (`acc_*`, `GOACC_*`) and GOMP_PLUGIN_*

These are GPU offloading APIs. ~65 symbols. Not relevant for this project.

### Not in scope: `_ull` variants

Unsigned long long variants of loop functions (for very large iteration counts).
~30 symbols. Same logic as regular variants, just wider types.

### Not in scope: Fortran `_` suffixed symbols

Fortran name-mangling variants (e.g., `omp_get_num_threads_`). ~50 symbols.
Same implementation, just different symbol name.

---

## Key Function: GOMP_parallel

This is the single most important function. GCC transforms:

```c
#pragma omp parallel
{
    body;
}
```

into:

```c
void outlined_fn(void *data) {
    body;  // captures are passed via data
}
...
GOMP_parallel(outlined_fn, &data, num_threads, flags);
```

**Signature:**
```c
void GOMP_parallel(void (*fn)(void *), void *data,
                   unsigned num_threads, unsigned flags);
```

**Semantics:**
1. Fork `num_threads` threads (or use existing pool)
2. Each thread calls `fn(data)` with its own thread ID set
3. Implicit barrier at the end — returns only when all threads done

**This is the entry point our GHC-backed runtime must implement first.**

---

## Key Function: GOMP_loop_static_start / _next

For `#pragma omp for schedule(static)`:

```c
bool GOMP_loop_static_start(long start, long end, long incr,
                            long chunk_size,
                            long *istart, long *iend);
bool GOMP_loop_static_next(long *istart, long *iend);
void GOMP_loop_end(void);
void GOMP_loop_end_nowait(void);
```

Each thread calls `_start` to get its first chunk `[*istart, *iend)`,
then repeatedly calls `_next` until it returns false.

---

## Summary: Implementation Priority

| Priority | Count | Category |
|---|---|---|
| **P0** | 9 | Symbols needed by basic test program |
| **P1** | ~25 | Core GOMP_1.0: parallel, loops, sync, omp_* basics |
| **P2** | ~15 | Tasks, sections, combined parallel loops |
| **P3** | ~15 | OMP 3.0 nesting/scheduling queries |
| **P4** | ~20 | OMP 4.0 cancel, doacross, target stubs |
| **P5** | ~15 | OMP 5.0 allocators, affinity, reductions |
| Skip | ~160 | OpenACC, Fortran, ULL variants, PLUGIN |

**Total for full OpenMP 4.5 coverage: ~85 symbols.**
**Minimum viable prototype: 9 symbols.**
