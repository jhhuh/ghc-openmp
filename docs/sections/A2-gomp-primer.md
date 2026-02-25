## Appendix: GOMP ABI Primer

GCC does not interpret OpenMP pragmas at runtime. Instead, the compiler
transforms each pragma into calls to `GOMP_*` functions at compile time.
Any library that exports these symbols can serve as the OpenMP runtime.

### Outlined Functions

GCC extracts the body of a parallel region into a separate *outlined*
function. The original code is replaced by a call to `GOMP_parallel`:

```c
// Source code:
#pragma omp parallel
{
    do_work(shared_data);
}

// GCC transforms this to:
static void outlined_fn(void *data) {
    struct shared *d = data;
    do_work(d->shared_data);
}
GOMP_parallel(outlined_fn, &shared_data, num_threads, flags);
```

`GOMP_parallel` calls `outlined_fn` on the master thread and dispatches
it to worker threads. An implicit barrier at the end ensures all threads
complete before the master returns.

### Worksharing Loops

`#pragma omp parallel for` combines a parallel region with work
distribution. GCC transforms the loop into a protocol:

```c
// Source code:
#pragma omp parallel for schedule(static)
for (int i = 0; i < n; i++)
    a[i] = compute(i);

// Each thread executes:
long start, end;
if (GOMP_loop_static_start(0, n, 1, chunk, &start, &end)) {
    do {
        for (long i = start; i < end; i++)
            a[i] = compute(i);
    } while (GOMP_loop_static_next(&start, &end));
}
GOMP_loop_end();
```

The `_start` function assigns the first chunk to the calling thread.
`_next` returns subsequent chunks until the iteration space is exhausted.
`_end` synchronizes with an implicit barrier.

For dynamic and guided scheduling, the same protocol applies with
`GOMP_loop_dynamic_start/_next` or `GOMP_loop_guided_start/_next`.
The runtime decides chunk sizes: static divides evenly, dynamic uses
fixed chunks, guided uses exponentially decreasing chunks.

### Reductions

OpenMP reductions use thread-local accumulators combined at the barrier:

```c
// Source code:
double sum = 0.0;
#pragma omp parallel for reduction(+:sum)
for (int i = 0; i < n; i++)
    sum += a[i];

// Each thread gets a local copy of sum (initialized to 0.0).
// At the barrier, all local copies are combined with +.
```

GCC generates the local copies and the combining code. The runtime
provides the barrier; the reduction logic is entirely compiler-generated.

### Critical Sections and Atomics

```c
GOMP_critical_start();    // acquire global mutex
  shared_counter++;
GOMP_critical_end();      // release global mutex

GOMP_critical_name_start(&named_lock);  // per-name mutex
  named_resource++;
GOMP_critical_name_end(&named_lock);
```

Unnamed critical sections share a single global mutex. Named critical
sections use per-name mutexes, allowing independent critical regions
to execute concurrently.

### Tasks

`GOMP_task` supports deferred execution:

```c
GOMP_task(fn, data, cpyfn, arg_size, arg_align, if_clause, flags, ...);
```

When `if_clause` is true and threads are available, the runtime copies
the task data (via `cpyfn` or `memcpy`) to the heap and enqueues it.
Idle threads steal tasks from other threads' queues. When `if_clause`
is false, the task executes immediately (inlined). `GOMP_taskwait`
blocks until all child tasks complete.

---

