/*
 * ghc_omp_runtime.c — Stub OpenMP runtime (libgomp ABI)
 *
 * Phase 1: Serial implementation that proves ABI compatibility.
 * Phase 2: Will be backed by GHC RTS capabilities.
 *
 * Build: gcc -shared -fPIC -o libgomp.so ghc_omp_runtime.c -lpthread
 * Usage: gcc -fopenmp -nostdlib-gomp test.c -L. -Wl,-rpath,. -lgomp
 *    or: LD_PRELOAD=./libgomp.so ./test_omp
 */

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>

/* =========================================================================
 * Internal state
 * ========================================================================= */

/* Maximum threads we support */
#define GHC_OMP_MAX_THREADS 128

/* Thread team state */
typedef struct {
    void (*fn)(void *);
    void *data;
    unsigned num_threads;

    /* Per-thread IDs */
    int thread_num;

    /* Barrier */
    pthread_barrier_t barrier;

    /* Critical section (default unnamed) */
    pthread_mutex_t critical_lock;
} omp_team_t;

/* Thread-local: which team am I in, and what's my ID? */
static __thread int tl_thread_num = 0;
static __thread int tl_num_threads = 1;
static __thread bool tl_in_parallel = false;

/* Global defaults */
static int g_num_threads = 0; /* 0 = use nprocs */
static pthread_mutex_t g_critical_lock = PTHREAD_MUTEX_INITIALIZER;

/* Single construct: global generation counter.
 * Each GOMP_single_start atomically increments. First to do so "wins." */
static volatile int g_single_generation = 0;
/* Per-thread: how many single regions this thread has seen */
static __thread int tl_single_generation = 0;

/* GOMP_barrier state */
static pthread_mutex_t g_barrier_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_barrier_cond = PTHREAD_COND_INITIALIZER;
static int g_barrier_count = 0;
static int g_barrier_generation = 0;

static int get_nprocs_count(void) {
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    return (n > 0) ? (int)n : 1;
}

static int get_env_num_threads(void) {
    const char *env = getenv("OMP_NUM_THREADS");
    if (env) {
        int n = atoi(env);
        if (n > 0) return n;
    }
    return 0;
}

static int resolve_num_threads(unsigned requested) {
    if (requested > 0) return (int)requested;
    if (g_num_threads > 0) return g_num_threads;
    int env = get_env_num_threads();
    if (env > 0) return env;
    return get_nprocs_count();
}

/* =========================================================================
 * Worker thread
 * ========================================================================= */

typedef struct {
    void (*fn)(void *);
    void *data;
    int thread_num;
    int num_threads;
    pthread_barrier_t *start_barrier; /* sync before fn() */
    pthread_barrier_t *end_barrier;   /* sync after fn() */
} worker_arg_t;

static void *worker_thread(void *arg) {
    worker_arg_t *w = (worker_arg_t *)arg;

    /* Set thread-local state */
    tl_thread_num = w->thread_num;
    tl_num_threads = w->num_threads;
    tl_in_parallel = true;
    tl_single_generation = 0;

    /* Wait for all threads (including master) to be ready */
    pthread_barrier_wait(w->start_barrier);

    /* Execute the outlined function */
    w->fn(w->data);

    /* Wait at implicit barrier */
    pthread_barrier_wait(w->end_barrier);

    tl_in_parallel = false;
    return NULL;
}

/* =========================================================================
 * GOMP_parallel — The core entry point
 *
 * void GOMP_parallel(void (*fn)(void *), void *data,
 *                    unsigned num_threads, unsigned flags);
 *
 * GCC transforms:
 *   #pragma omp parallel
 *   { body; }
 * into:
 *   void outlined(void *data) { body; }
 *   GOMP_parallel(outlined, &captures, nthreads, flags);
 * ========================================================================= */

void GOMP_parallel(void (*fn)(void *), void *data,
                   unsigned num_threads, unsigned flags) {
    (void)flags;

    int nthreads = resolve_num_threads(num_threads);
    if (nthreads < 1) nthreads = 1;
    if (nthreads > GHC_OMP_MAX_THREADS) nthreads = GHC_OMP_MAX_THREADS;

    pthread_barrier_t start_barrier, end_barrier;
    pthread_barrier_init(&start_barrier, NULL, nthreads);
    pthread_barrier_init(&end_barrier, NULL, nthreads);

    /* Reset per-region global state before any thread runs fn */
    g_single_generation = 0;
    g_barrier_count = 0;

    pthread_t threads[GHC_OMP_MAX_THREADS];
    worker_arg_t args[GHC_OMP_MAX_THREADS];

    /* Launch worker threads 1..N-1 */
    for (int i = 1; i < nthreads; i++) {
        args[i].fn = fn;
        args[i].data = data;
        args[i].thread_num = i;
        args[i].num_threads = nthreads;
        args[i].start_barrier = &start_barrier;
        args[i].end_barrier = &end_barrier;
        pthread_create(&threads[i], NULL, worker_thread, &args[i]);
    }

    /* Master thread (thread 0) — set state then sync at start barrier */
    int saved_thread_num = tl_thread_num;
    int saved_num_threads = tl_num_threads;
    bool saved_in_parallel = tl_in_parallel;

    tl_thread_num = 0;
    tl_num_threads = nthreads;
    tl_in_parallel = true;
    tl_single_generation = 0;

    /* All threads sync here — everyone has initialized TLS */
    pthread_barrier_wait(&start_barrier);

    fn(data);

    /* Implicit end-of-parallel barrier */
    pthread_barrier_wait(&end_barrier);

    /* Join all worker threads */
    for (int i = 1; i < nthreads; i++) {
        pthread_join(threads[i], NULL);
    }

    /* Restore master thread state */
    tl_thread_num = saved_thread_num;
    tl_num_threads = saved_num_threads;
    tl_in_parallel = saved_in_parallel;

    pthread_barrier_destroy(&start_barrier);
    pthread_barrier_destroy(&end_barrier);
}

/* Legacy split API */
static pthread_barrier_t g_legacy_start_barrier;
static pthread_barrier_t g_legacy_end_barrier;
static pthread_t g_legacy_threads[GHC_OMP_MAX_THREADS];
static worker_arg_t g_legacy_args[GHC_OMP_MAX_THREADS];
static int g_legacy_nthreads = 0;

void GOMP_parallel_start(void (*fn)(void *), void *data,
                         unsigned num_threads) {
    int nthreads = resolve_num_threads(num_threads);
    if (nthreads < 1) nthreads = 1;
    if (nthreads > GHC_OMP_MAX_THREADS) nthreads = GHC_OMP_MAX_THREADS;
    g_legacy_nthreads = nthreads;

    g_single_generation = 0;

    pthread_barrier_init(&g_legacy_start_barrier, NULL, nthreads);
    pthread_barrier_init(&g_legacy_end_barrier, NULL, nthreads);

    for (int i = 1; i < nthreads; i++) {
        g_legacy_args[i].fn = fn;
        g_legacy_args[i].data = data;
        g_legacy_args[i].thread_num = i;
        g_legacy_args[i].num_threads = nthreads;
        g_legacy_args[i].start_barrier = &g_legacy_start_barrier;
        g_legacy_args[i].end_barrier = &g_legacy_end_barrier;
        pthread_create(&g_legacy_threads[i], NULL, worker_thread,
                       &g_legacy_args[i]);
    }

    tl_thread_num = 0;
    tl_num_threads = nthreads;
    tl_in_parallel = true;
    tl_single_generation = 0;
    pthread_barrier_wait(&g_legacy_start_barrier);
}

void GOMP_parallel_end(void) {
    pthread_barrier_wait(&g_legacy_end_barrier);
    for (int i = 1; i < g_legacy_nthreads; i++) {
        pthread_join(g_legacy_threads[i], NULL);
    }
    pthread_barrier_destroy(&g_legacy_start_barrier);
    pthread_barrier_destroy(&g_legacy_end_barrier);

    tl_thread_num = 0;
    tl_num_threads = 1;
    tl_in_parallel = false;
    g_legacy_nthreads = 0;
}

/* =========================================================================
 * GOMP_barrier
 * ========================================================================= */

/* For a real implementation, each parallel region should have its own barrier.
 * For now, we use a simple sense-reversing barrier keyed on the thread team.
 *
 * HACK: This stub uses a global pthread_barrier. In a real impl, the barrier
 * would be associated with the current team. For now, since we create/destroy
 * the barrier in GOMP_parallel, we need a separate mechanism here.
 *
 * We'll use a counting barrier with atomics.
 */
void GOMP_barrier(void) {
    int nthreads = tl_num_threads;
    if (nthreads <= 1) return;

    pthread_mutex_lock(&g_barrier_mutex);
    int gen = g_barrier_generation;
    g_barrier_count++;
    if (g_barrier_count >= nthreads) {
        g_barrier_count = 0;
        g_barrier_generation++;
        pthread_cond_broadcast(&g_barrier_cond);
    } else {
        while (gen == g_barrier_generation) {
            pthread_cond_wait(&g_barrier_cond, &g_barrier_mutex);
        }
    }
    pthread_mutex_unlock(&g_barrier_mutex);
}

/* =========================================================================
 * GOMP_critical_start / GOMP_critical_end
 * ========================================================================= */

void GOMP_critical_start(void) {
    pthread_mutex_lock(&g_critical_lock);
}

void GOMP_critical_end(void) {
    pthread_mutex_unlock(&g_critical_lock);
}

void GOMP_critical_name_start(void **pptr) {
    /* Each named critical section gets its own mutex, lazily allocated */
    if (*pptr == NULL) {
        pthread_mutex_t *m = malloc(sizeof(pthread_mutex_t));
        pthread_mutex_init(m, NULL);
        /* Race: two threads could both allocate. Use CAS. */
        if (!__sync_bool_compare_and_swap(pptr, NULL, m)) {
            pthread_mutex_destroy(m);
            free(m);
        }
    }
    pthread_mutex_lock((pthread_mutex_t *)*pptr);
}

void GOMP_critical_name_end(void **pptr) {
    pthread_mutex_unlock((pthread_mutex_t *)*pptr);
}

/* =========================================================================
 * GOMP_atomic_start / GOMP_atomic_end
 * Fallback for non-natively-atomic operations
 * ========================================================================= */

static pthread_mutex_t g_atomic_lock = PTHREAD_MUTEX_INITIALIZER;

void GOMP_atomic_start(void) {
    pthread_mutex_lock(&g_atomic_lock);
}

void GOMP_atomic_end(void) {
    pthread_mutex_unlock(&g_atomic_lock);
}

/* =========================================================================
 * GOMP_single_start
 * Returns true for exactly one thread in the team.
 * ========================================================================= */

bool GOMP_single_start(void) {
    /* Each thread tracks how many single regions it has entered.
     * The first thread to CAS the generation counter from my_gen to my_gen+1 wins.
     * All other threads see it already incremented and lose. */
    int my_gen = tl_single_generation;
    tl_single_generation++;
    return __sync_bool_compare_and_swap(&g_single_generation, my_gen, my_gen + 1);
}

void *GOMP_single_copy_start(void) {
    /* Simplified: always returns NULL (master executes) */
    if (tl_thread_num == 0) return NULL;
    /* Other threads need to wait and receive the data pointer */
    /* For now, return a sentinel */
    return NULL;
}

void GOMP_single_copy_end(void *data) {
    (void)data;
    /* In a real implementation, broadcast `data` to other threads */
}

/* =========================================================================
 * GOMP_ordered_start / GOMP_ordered_end
 * ========================================================================= */

static pthread_mutex_t g_ordered_lock = PTHREAD_MUTEX_INITIALIZER;

void GOMP_ordered_start(void) {
    pthread_mutex_lock(&g_ordered_lock);
}

void GOMP_ordered_end(void) {
    pthread_mutex_unlock(&g_ordered_lock);
}

/* =========================================================================
 * GOMP_loop_static_start / _next / _end
 * Static worksharing loop
 * ========================================================================= */

/* Per-thread loop state */
static __thread long tl_loop_start;
static __thread long tl_loop_end;
static __thread long tl_loop_incr;
static __thread long tl_loop_chunk;
static __thread long tl_loop_cur;
static __thread long tl_loop_own_end;

bool GOMP_loop_static_start(long start, long end, long incr,
                            long chunk_size, long *istart, long *iend) {
    long total = (end - start + incr - (incr > 0 ? 1 : -1)) / incr;
    if (total <= 0) return false;

    int tid = tl_thread_num;
    int nthreads = tl_num_threads;

    /* Default chunk: divide evenly */
    if (chunk_size <= 0) chunk_size = (total + nthreads - 1) / nthreads;

    long my_start_iter = tid * chunk_size;
    if (my_start_iter >= total) return false;

    long my_end_iter = my_start_iter + chunk_size;
    if (my_end_iter > total) my_end_iter = total;

    *istart = start + my_start_iter * incr;
    *iend = start + my_end_iter * incr;

    /* Save state for _next calls */
    tl_loop_start = start;
    tl_loop_end = end;
    tl_loop_incr = incr;
    tl_loop_chunk = chunk_size;
    tl_loop_cur = my_end_iter;
    tl_loop_own_end = total;

    return true;
}

bool GOMP_loop_static_next(long *istart, long *iend) {
    /* For simple static, there's only one chunk per thread */
    /* With chunk_size < total/nthreads, there could be round-robin */
    long total = tl_loop_own_end;
    int nthreads = tl_num_threads;
    long next_start = tl_loop_cur + (nthreads - 1) * tl_loop_chunk;

    if (next_start >= total) return false;

    long next_end = next_start + tl_loop_chunk;
    if (next_end > total) next_end = total;

    *istart = tl_loop_start + next_start * tl_loop_incr;
    *iend = tl_loop_start + next_end * tl_loop_incr;

    tl_loop_cur = next_end;
    return true;
}

/* Forward declaration */
bool GOMP_loop_dynamic_next(long *istart, long *iend);

/* Dynamic loop: uses atomic counter */
static long g_dynamic_next_iter = 0;
static long g_dynamic_total = 0;
static long g_dynamic_start = 0;
static long g_dynamic_incr = 1;
static long g_dynamic_chunk = 1;

bool GOMP_loop_dynamic_start(long start, long end, long incr,
                             long chunk_size, long *istart, long *iend) {
    long total = (end - start + incr - (incr > 0 ? 1 : -1)) / incr;
    if (total <= 0) return false;

    if (chunk_size <= 0) chunk_size = 1;

    /* Initialize global state (race: only one thread should do this).
     * HACK: in a real impl this is per-workshare. */
    if (tl_thread_num == 0) {
        g_dynamic_next_iter = 0;
        g_dynamic_total = total;
        g_dynamic_start = start;
        g_dynamic_incr = incr;
        g_dynamic_chunk = chunk_size;
    }
    /* Need a barrier here so all threads see the initialization */
    GOMP_barrier();

    return GOMP_loop_dynamic_next(istart, iend);
}

bool GOMP_loop_dynamic_next(long *istart, long *iend) {
    long chunk = g_dynamic_chunk;
    long mine = __sync_fetch_and_add(&g_dynamic_next_iter, chunk);

    if (mine >= g_dynamic_total) return false;

    long end = mine + chunk;
    if (end > g_dynamic_total) end = g_dynamic_total;

    *istart = g_dynamic_start + mine * g_dynamic_incr;
    *iend = g_dynamic_start + end * g_dynamic_incr;
    return true;
}

/* Guided and runtime: delegate to dynamic for now */
bool GOMP_loop_guided_start(long start, long end, long incr,
                            long chunk_size, long *istart, long *iend) {
    return GOMP_loop_dynamic_start(start, end, incr, chunk_size, istart, iend);
}

bool GOMP_loop_guided_next(long *istart, long *iend) {
    return GOMP_loop_dynamic_next(istart, iend);
}

bool GOMP_loop_runtime_start(long start, long end, long incr,
                             long chunk_size, long *istart, long *iend) {
    /* Default to static */
    return GOMP_loop_static_start(start, end, incr, chunk_size, istart, iend);
}

bool GOMP_loop_runtime_next(long *istart, long *iend) {
    return GOMP_loop_static_next(istart, iend);
}

/* Loop end (implicit barrier) and nowait variant */
void GOMP_loop_end(void) {
    GOMP_barrier();
}

void GOMP_loop_end_nowait(void) {
    /* No barrier */
}

/* Combined parallel + loop */
void GOMP_parallel_loop_static(void (*fn)(void *), void *data,
                               unsigned num_threads, long start, long end,
                               long incr, long chunk_size, unsigned flags) {
    (void)start; (void)end; (void)incr; (void)chunk_size; (void)flags;
    GOMP_parallel(fn, data, num_threads, flags);
}

void GOMP_parallel_loop_dynamic(void (*fn)(void *), void *data,
                                unsigned num_threads, long start, long end,
                                long incr, long chunk_size, unsigned flags) {
    (void)start; (void)end; (void)incr; (void)chunk_size; (void)flags;
    GOMP_parallel(fn, data, num_threads, flags);
}

void GOMP_parallel_loop_guided(void (*fn)(void *), void *data,
                               unsigned num_threads, long start, long end,
                               long incr, long chunk_size, unsigned flags) {
    (void)start; (void)end; (void)incr; (void)chunk_size; (void)flags;
    GOMP_parallel(fn, data, num_threads, flags);
}

void GOMP_parallel_loop_runtime(void (*fn)(void *), void *data,
                                unsigned num_threads, long start, long end,
                                long incr, long chunk_size, unsigned flags) {
    (void)start; (void)end; (void)incr; (void)chunk_size; (void)flags;
    GOMP_parallel(fn, data, num_threads, flags);
}

/* =========================================================================
 * GOMP_task / GOMP_taskwait
 * Simplified: tasks execute inline (no deferred execution)
 * ========================================================================= */

void GOMP_task(void (*fn)(void *), void *data,
               void (*cpyfn)(void *, void *),
               long arg_size, long arg_align,
               bool if_clause, unsigned flags,
               void **depend, int priority) {
    (void)cpyfn; (void)arg_size; (void)arg_align;
    (void)flags; (void)depend; (void)priority;

    if (if_clause && cpyfn) {
        /* Need to copy the data for deferred execution */
        void *buf = malloc(arg_size);
        cpyfn(buf, data);
        fn(buf);
        free(buf);
    } else {
        /* Execute inline */
        fn(data);
    }
}

void GOMP_taskwait(void) {
    /* Since tasks execute inline, nothing to wait for */
}

void GOMP_taskyield(void) {
    /* Yield current task — no-op in serial task model */
}

void GOMP_taskgroup_start(void) {
    /* No-op in serial task model */
}

void GOMP_taskgroup_end(void) {
    /* Implicit taskwait — no-op */
}

/* =========================================================================
 * GOMP_sections
 * ========================================================================= */

static int g_sections_next = 0;
static int g_sections_count = 0;

unsigned GOMP_sections_start(unsigned count) {
    g_sections_count = count;
    g_sections_next = 1; /* section 0 is returned to caller */
    return 0;
}

unsigned GOMP_sections_next(void) {
    int s = __sync_fetch_and_add(&g_sections_next, 1);
    if (s >= g_sections_count) return 0;
    return s;
}

void GOMP_sections_end(void) {
    GOMP_barrier();
}

void GOMP_sections_end_nowait(void) {
    /* No barrier */
}

void GOMP_parallel_sections(void (*fn)(void *), void *data,
                            unsigned num_threads, unsigned count,
                            unsigned flags) {
    (void)count; (void)flags;
    GOMP_parallel(fn, data, num_threads, flags);
}

/* =========================================================================
 * GOMP_loop_start (GOMP_5.0 unified entry)
 * ========================================================================= */

bool GOMP_loop_start(long start, long end, long incr, long chunk_size,
                     long *istart, long *iend, void **reductions,
                     void **mem) {
    (void)reductions; (void)mem;
    return GOMP_loop_static_start(start, end, incr, chunk_size, istart, iend);
}

/* =========================================================================
 * omp_* user API
 * ========================================================================= */

int omp_get_num_threads(void) {
    return tl_num_threads;
}

int omp_get_thread_num(void) {
    return tl_thread_num;
}

int omp_get_max_threads(void) {
    if (g_num_threads > 0) return g_num_threads;
    return get_nprocs_count();
}

int omp_get_num_procs(void) {
    return get_nprocs_count();
}

void omp_set_num_threads(int n) {
    if (n > 0) g_num_threads = n;
}

int omp_in_parallel(void) {
    return tl_in_parallel ? 1 : 0;
}

void omp_set_dynamic(int val) {
    (void)val; /* Ignore — we don't adjust thread count dynamically */
}

int omp_get_dynamic(void) {
    return 0;
}

void omp_set_nested(int val) {
    (void)val; /* Nested parallelism not supported */
}

int omp_get_nested(void) {
    return 0;
}

double omp_get_wtime(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec * 1e-9;
}

double omp_get_wtick(void) {
    struct timespec ts;
    clock_getres(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec * 1e-9;
}

/* ---- Locks ---- */

typedef pthread_mutex_t omp_lock_t;

void omp_init_lock(omp_lock_t *lock) {
    pthread_mutex_init(lock, NULL);
}

void omp_destroy_lock(omp_lock_t *lock) {
    pthread_mutex_destroy(lock);
}

void omp_set_lock(omp_lock_t *lock) {
    pthread_mutex_lock(lock);
}

void omp_unset_lock(omp_lock_t *lock) {
    pthread_mutex_unlock(lock);
}

int omp_test_lock(omp_lock_t *lock) {
    return pthread_mutex_trylock(lock) == 0 ? 1 : 0;
}

/* Nested locks use a recursive mutex */
typedef pthread_mutex_t omp_nest_lock_t;

void omp_init_nest_lock(omp_nest_lock_t *lock) {
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(lock, &attr);
    pthread_mutexattr_destroy(&attr);
}

void omp_destroy_nest_lock(omp_nest_lock_t *lock) {
    pthread_mutex_destroy(lock);
}

void omp_set_nest_lock(omp_nest_lock_t *lock) {
    pthread_mutex_lock(lock);
}

void omp_unset_nest_lock(omp_nest_lock_t *lock) {
    pthread_mutex_unlock(lock);
}

int omp_test_nest_lock(omp_nest_lock_t *lock) {
    return pthread_mutex_trylock(lock) == 0 ? 1 : 0;
}

/* ---- OpenMP 3.0+ stubs ---- */

int omp_get_level(void) { return tl_in_parallel ? 1 : 0; }
int omp_get_active_level(void) { return tl_in_parallel ? 1 : 0; }
int omp_get_ancestor_thread_num(int level) {
    (void)level;
    return (level == 1) ? tl_thread_num : 0;
}
int omp_get_team_size(int level) {
    (void)level;
    return (level == 1) ? tl_num_threads : 1;
}
int omp_get_thread_limit(void) { return GHC_OMP_MAX_THREADS; }
void omp_set_max_active_levels(int val) { (void)val; }
int omp_get_max_active_levels(void) { return 1; }
int omp_get_supported_active_levels(void) { return 1; }

/* Schedule */
typedef enum { omp_sched_static=1, omp_sched_dynamic=2,
               omp_sched_guided=3, omp_sched_auto=4 } omp_sched_t;
static omp_sched_t g_schedule = omp_sched_static;
static int g_schedule_chunk = 0;

void omp_set_schedule(omp_sched_t kind, int chunk) {
    g_schedule = kind;
    g_schedule_chunk = chunk;
}
void omp_get_schedule(omp_sched_t *kind, int *chunk) {
    *kind = g_schedule;
    *chunk = g_schedule_chunk;
}

int omp_in_final(void) { return 0; }
int omp_get_cancellation(void) { return 0; }
int omp_get_proc_bind(void) { return 0; /* omp_proc_bind_false */ }
int omp_get_num_places(void) { return 0; }
int omp_get_place_num(void) { return -1; }
int omp_get_default_device(void) { return 0; }
void omp_set_default_device(int d) { (void)d; }
int omp_get_num_devices(void) { return 0; }
int omp_get_num_teams(void) { return 1; }
int omp_get_team_num(void) { return 0; }
int omp_is_initial_device(void) { return 1; }
int omp_get_initial_device(void) { return -1; }
int omp_get_max_task_priority(void) { return 0; }

/* ---- Cancel stubs ---- */

void GOMP_cancel(int which, bool do_cancel) { (void)which; (void)do_cancel; }
void GOMP_cancellation_point(int which) { (void)which; }
void GOMP_barrier_cancel(void) {}
void GOMP_loop_end_cancel(void) {}
void GOMP_sections_end_cancel(void) {}

/* ---- Teams stubs ---- */

void GOMP_teams_reg(void (*fn)(void *), void *data, unsigned num_teams,
                    unsigned thread_limit, unsigned flags) {
    (void)num_teams; (void)thread_limit; (void)flags;
    fn(data);
}
