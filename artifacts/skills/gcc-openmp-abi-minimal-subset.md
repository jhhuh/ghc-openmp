# GCC OpenMP (GOMP) ABI: minimal implementation subset

## Context

A basic OpenMP program using `parallel`, `for`, `critical`, `single`,
`barrier`, and `task` only needs 9 GOMP symbols. The full libgomp exports ~280,
but ~160 are OpenACC/Fortran/ULL variants.

## Minimal 9 symbols

```c
void GOMP_parallel(void (*fn)(void*), void *data, unsigned num_threads, unsigned flags);
bool GOMP_single_start(void);
void GOMP_barrier(void);
void GOMP_critical_start(void);
void GOMP_critical_end(void);
bool GOMP_loop_dynamic_start(long start, long end, long incr, long chunk, long *istart, long *iend);
bool GOMP_loop_dynamic_next(long *istart, long *iend);
void GOMP_loop_end_nowait(void);
void GOMP_task(void (*fn)(void*), void *data, void (*cpyfn)(void*,void*), long arg_size, long arg_align, bool if_clause, unsigned flags, void **depend, int priority);
```

## How to discover needed symbols

```bash
gcc -fopenmp -O2 -c my_omp_code.c
nm my_omp_code.o | grep ' U GOMP\|U omp_'
```

## Key discoveries

1. GCC 15 emits `GOMP_loop_nonmonotonic_*` variants for `schedule(dynamic/guided)`.
2. Static schedule is compiler-side — no `GOMP_loop_static_*` at runtime.
3. `GOMP_task` cpyfn can be NULL for simple scalar firstprivate vars.
4. GCC may omit `GOMP_barrier` after `#pragma omp single`.
