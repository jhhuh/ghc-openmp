/*
 * ghc_omp_runtime_rts.c — OpenMP runtime backed by GHC RTS Capabilities
 *
 * Phase 2: Replaces pthread_create with GHC RTS Capabilities.
 * Worker OS threads are pinned to Capabilities via rts_setInCallCapability().
 * Each worker's "thread ID" is its Capability number (cap->no).
 *
 * Build: requires linking via ghc -threaded -no-hs-main with HsStub.o
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>
#include <stdatomic.h>

#ifdef GHC_OMP_DEBUG
#define DBG(fmt, ...) fprintf(stderr, "[ghcomp] " fmt "\n", ##__VA_ARGS__)
#else
#define DBG(fmt, ...) ((void)0)
#endif

/* GHC RTS headers (need -DTHREADED_RTS -I<rts-include-dir>) */
#include "Rts.h"
#include "RtsAPI.h"

/* =========================================================================
 * Configuration
 * ========================================================================= */

#define GHC_OMP_MAX_THREADS 128
#define SPIN_ITERS 4000  /* spin iterations before falling back to condvar */

/* =========================================================================
 * Sense-reversing centralized barrier (lock-free)
 *
 * Each thread has a local sense flag. Threads atomically decrement a
 * shared counter. The last thread flips the global sense, releasing all.
 * ========================================================================= */

typedef struct {
    atomic_int count;      /* threads remaining */
    atomic_int sense;      /* global sense flag */
    int size;              /* team size (reset value) */
} spin_barrier_t;

static inline void spin_barrier_init(spin_barrier_t *b, int n) {
    atomic_store_explicit(&b->count, n, memory_order_relaxed);
    atomic_store_explicit(&b->sense, 0, memory_order_relaxed);
    b->size = n;
}

static inline void spin_barrier_wait(spin_barrier_t *b, int *local_sense) {
    *local_sense = 1 - *local_sense;
    if (atomic_fetch_sub_explicit(&b->count, 1, memory_order_acq_rel) == 1) {
        /* Last thread: reset counter and flip sense */
        atomic_store_explicit(&b->count, b->size, memory_order_relaxed);
        atomic_store_explicit(&b->sense, *local_sense, memory_order_release);
    } else {
        /* Spin until sense flips */
        while (atomic_load_explicit(&b->sense, memory_order_acquire) != *local_sense) {
            __builtin_ia32_pause();
        }
    }
}

/* =========================================================================
 * Internal state
 * ========================================================================= */

/* Thread-local OpenMP state */
static __thread int tl_thread_num = 0;
static __thread int tl_num_threads = 1;
static __thread bool tl_in_parallel = false;

/* Single construct */
static __thread int tl_single_generation = 0;
static volatile int g_single_generation = 0;

/* GOMP_barrier — atomic sense-reversing barrier */
static spin_barrier_t g_omp_barrier;
static __thread int tl_barrier_sense = 0;

/* Global defaults */
static int g_num_threads = 0; /* 0 = use nprocs / OMP_NUM_THREADS */
static pthread_mutex_t g_critical_lock = PTHREAD_MUTEX_INITIALIZER;

/* GHC RTS initialization state */
static bool g_rts_initialized = false;
static pthread_once_t g_rts_init_once = PTHREAD_ONCE_INIT;
static int g_rts_num_caps = 0; /* actual number of Capabilities after init */

/* =========================================================================
 * Worker pool — persistent OS threads pinned to GHC Capabilities
 *
 * Lock-free work dispatch using atomic generation counter.
 * Workers spin on generation change (no mutex on the hot path).
 * ========================================================================= */

typedef struct {
    /* Work item — written by master before generation increment */
    void (*fn)(void *);
    void *data;
    int num_threads;           /* team size for this parallel region */

    /* Lock-free synchronization */
    atomic_int generation;     /* incremented each GOMP_parallel call */
    atomic_int workers_done;   /* completion counter */
    atomic_bool active;        /* false = shutdown */

    spin_barrier_t start_barrier;  /* sync before fn() */
    spin_barrier_t end_barrier;    /* sync after fn() */

    /* Fallback mutex+condvar for idle sleep (power efficiency) */
    pthread_mutex_t sleep_mutex;
    pthread_cond_t sleep_cond;
} worker_pool_t;

static pthread_t g_worker_threads[GHC_OMP_MAX_THREADS];
static worker_pool_t g_pool;
static int g_pool_size = 0; /* number of workers (excluding master = thread 0) */
static pthread_barrier_t g_init_barrier; /* workers signal when rts_lock/unlock done */

static void *worker_loop(void *arg) {
    int worker_id = (int)(long)arg; /* 1-indexed: worker i handles cap i */

    DBG("worker %d: starting, pinning to cap %d", worker_id, worker_id);

    /* Pin this OS thread to Capability `worker_id` */
    rts_setInCallCapability(worker_id, 1);

    DBG("worker %d: calling rts_lock()", worker_id);

    /* Acquire then immediately release capability to register with RTS */
    Capability *cap = rts_lock();
    DBG("worker %d: got cap, calling rts_unlock()", worker_id);
    rts_unlock(cap);

    /* Signal that this worker is ready */
    pthread_barrier_wait(&g_init_barrier);

    DBG("worker %d: entering work loop", worker_id);

    int my_gen = 0; /* track which generation we've processed */
    int start_sense = 0, end_sense = 0; /* per-thread sense for barriers */

    while (1) {
        /* Spin-wait for new work (generation change) */
        int new_gen;
        int spins = 0;
        while (1) {
            new_gen = atomic_load_explicit(&g_pool.generation,
                                           memory_order_acquire);
            if (new_gen != my_gen) break;
            if (!atomic_load_explicit(&g_pool.active,
                                      memory_order_relaxed)) goto done;
            if (++spins < SPIN_ITERS) {
                __builtin_ia32_pause();
            } else {
                /* Fall back to condvar for power efficiency */
                pthread_mutex_lock(&g_pool.sleep_mutex);
                /* Re-check under lock */
                new_gen = atomic_load_explicit(&g_pool.generation,
                                               memory_order_acquire);
                if (new_gen != my_gen ||
                    !atomic_load_explicit(&g_pool.active,
                                          memory_order_relaxed)) {
                    pthread_mutex_unlock(&g_pool.sleep_mutex);
                    if (!atomic_load_explicit(&g_pool.active,
                                              memory_order_relaxed)
                        && new_gen == my_gen) goto done;
                    break;
                }
                pthread_cond_wait(&g_pool.sleep_cond,
                                  &g_pool.sleep_mutex);
                pthread_mutex_unlock(&g_pool.sleep_mutex);
                spins = 0; /* restart spin after wakeup */
            }
        }

        my_gen = new_gen;

        /* Read work item (safe: master wrote before generation store) */
        void (*fn)(void *) = g_pool.fn;
        void *data = g_pool.data;
        int num_threads = g_pool.num_threads;
        DBG("worker %d: gen=%d, num_threads=%d", worker_id, my_gen, num_threads);

        /* Only participate if our ID < team size */
        if (worker_id < num_threads) {
            tl_thread_num = worker_id;
            tl_num_threads = num_threads;
            tl_in_parallel = true;
            tl_single_generation = 0;
            tl_barrier_sense = 0; /* reset to match g_omp_barrier */
            start_sense = 0; /* reset to match re-initialized start_barrier */
            end_sense = 0;   /* reset to match re-initialized end_barrier */

            /* Sync: all team members ready */
            spin_barrier_wait(&g_pool.start_barrier, &start_sense);

            /* Execute */
            fn(data);

            /* End barrier */
            spin_barrier_wait(&g_pool.end_barrier, &end_sense);

            tl_in_parallel = false;

            /* Signal completion (atomic, no mutex) */
            atomic_fetch_add_explicit(&g_pool.workers_done, 1,
                                      memory_order_release);
        }
    }
done:
    return NULL;
}

/* =========================================================================
 * RTS initialization
 * ========================================================================= */

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
    return (int)getNumberOfProcessors();
}

static void init_rts(void) {
    /* Determine the number of capabilities */
    int ncaps = resolve_num_threads(0);
    if (ncaps < 1) ncaps = 1;
    if (ncaps > GHC_OMP_MAX_THREADS) ncaps = GHC_OMP_MAX_THREADS;

    DBG("init_rts: ncaps=%d", ncaps);

    /* Build RTS options string */
    char rts_opts[64];
    snprintf(rts_opts, sizeof(rts_opts), "-N%d", ncaps);

    /* Boot the GHC RTS */
    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    conf.rts_hs_main = HS_BOOL_FALSE;
    conf.rts_opts = rts_opts;

    int argc = 1;
    char *argv_arr[] = { "ghc-omp", NULL };
    char **argv = argv_arr;

    DBG("init_rts: calling hs_init_ghc()");
    hs_init_ghc(&argc, &argv, conf);
    DBG("init_rts: hs_init_ghc() returned");

    g_rts_num_caps = getNumCapabilities();
    g_rts_initialized = true;

    DBG("init_rts: %d capabilities available", g_rts_num_caps);

    /* Initialize worker pool */
    g_pool_size = g_rts_num_caps - 1; /* workers are caps 1..N-1 */
    pthread_mutex_init(&g_pool.sleep_mutex, NULL);
    pthread_cond_init(&g_pool.sleep_cond, NULL);
    atomic_store(&g_pool.active, true);
    atomic_store(&g_pool.generation, 0);
    atomic_store(&g_pool.workers_done, 0);

    /* Pin master thread (thread 0) to Capability 0 */
    rts_setInCallCapability(0, 1);

    DBG("init_rts: spawning %d worker threads", g_pool_size);

    /* Init barrier: workers + master = pool_size + 1 */
    pthread_barrier_init(&g_init_barrier, NULL, g_pool_size + 1);

    /* Spawn worker threads (one per Capability 1..N-1) */
    for (int i = 0; i < g_pool_size; i++) {
        int worker_id = i + 1;
        pthread_create(&g_worker_threads[i], NULL, worker_loop,
                       (void *)(long)worker_id);
    }

    /* Wait for all workers to finish rts_lock/unlock registration */
    DBG("init_rts: waiting for workers to register with RTS");
    pthread_barrier_wait(&g_init_barrier);
    pthread_barrier_destroy(&g_init_barrier);

    DBG("init_rts: done, all workers ready");
}

static void ensure_rts(void) {
    pthread_once(&g_rts_init_once, init_rts);
}

/* Destructor: clean shutdown */
__attribute__((destructor))
static void shutdown_rts(void) {
    if (!g_rts_initialized) return;

    /* Signal all workers to exit */
    atomic_store_explicit(&g_pool.active, false, memory_order_release);
    /* Wake any workers sleeping on condvar */
    pthread_mutex_lock(&g_pool.sleep_mutex);
    pthread_cond_broadcast(&g_pool.sleep_cond);
    pthread_mutex_unlock(&g_pool.sleep_mutex);

    for (int i = 0; i < g_pool_size; i++) {
        pthread_join(g_worker_threads[i], NULL);
    }

    hs_exit();
    g_rts_initialized = false;
}

/* =========================================================================
 * GOMP_parallel — dispatch to GHC Capabilities
 * ========================================================================= */

void GOMP_parallel(void (*fn)(void *), void *data,
                   unsigned num_threads, unsigned flags) {
    (void)flags;
    DBG("GOMP_parallel: enter, num_threads=%u", num_threads);
    ensure_rts();

    int nthreads = resolve_num_threads(num_threads);
    if (nthreads < 1) nthreads = 1;
    if (nthreads > g_rts_num_caps) nthreads = g_rts_num_caps;

    DBG("GOMP_parallel: nthreads=%d, pool_size=%d", nthreads, g_pool_size);

    /* Save/set master thread-local state */
    int saved_thread_num = tl_thread_num;
    int saved_num_threads = tl_num_threads;
    bool saved_in_parallel = tl_in_parallel;

    tl_thread_num = 0;
    tl_num_threads = nthreads;
    tl_in_parallel = true;
    tl_single_generation = 0;
    tl_barrier_sense = 0; /* reset to match g_omp_barrier */

    /* Reset per-region state */
    g_single_generation = 0;
    spin_barrier_init(&g_omp_barrier, nthreads);

    if (nthreads == 1) {
        /* Single-thread fast path: no pool interaction needed */
        DBG("GOMP_parallel: single-thread fast path");
        fn(data);
    } else {
        /* Multi-thread: use worker pool */
        int num_workers = nthreads - 1;

        /* Initialize spin barriers for this team */
        spin_barrier_init(&g_pool.start_barrier, nthreads);
        spin_barrier_init(&g_pool.end_barrier, nthreads);

        /* Store work item, then advance generation (release fence) */
        g_pool.fn = fn;
        g_pool.data = data;
        g_pool.num_threads = nthreads;
        atomic_store_explicit(&g_pool.workers_done, 0,
                              memory_order_relaxed);
        atomic_fetch_add_explicit(&g_pool.generation, 1,
                                  memory_order_release);

        /* Wake any workers sleeping on condvar fallback */
        pthread_mutex_lock(&g_pool.sleep_mutex);
        pthread_cond_broadcast(&g_pool.sleep_cond);
        pthread_mutex_unlock(&g_pool.sleep_mutex);

        /* Master barrier senses — always start at 0 to match freshly
         * initialized barriers (sense=0). Not persistent across calls. */
        int master_start_sense = 0;
        int master_end_sense = 0;

        spin_barrier_wait(&g_pool.start_barrier, &master_start_sense);

        /* Execute */
        fn(data);

        spin_barrier_wait(&g_pool.end_barrier, &master_end_sense);

        /* Spin-wait for all workers to signal done */
        while (atomic_load_explicit(&g_pool.workers_done,
                                    memory_order_acquire) < num_workers) {
            __builtin_ia32_pause();
        }
    }

    /* Restore */
    tl_thread_num = saved_thread_num;
    tl_num_threads = saved_num_threads;
    tl_in_parallel = saved_in_parallel;
}

/* Legacy split API */
void GOMP_parallel_start(void (*fn)(void *), void *data,
                         unsigned num_threads) {
    /* For legacy, just delegate to the unified version */
    /* This is a simplification — ideally the master would return immediately */
    (void)fn; (void)data; (void)num_threads;
    ensure_rts();
    tl_thread_num = 0;
    tl_num_threads = resolve_num_threads(num_threads);
    tl_in_parallel = true;
}

void GOMP_parallel_end(void) {
    tl_thread_num = 0;
    tl_num_threads = 1;
    tl_in_parallel = false;
}

/* =========================================================================
 * GOMP_barrier
 * ========================================================================= */

void GOMP_barrier(void) {
    if (tl_num_threads <= 1) return;
    spin_barrier_wait(&g_omp_barrier, &tl_barrier_sense);
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
    if (*pptr == NULL) {
        pthread_mutex_t *m = malloc(sizeof(pthread_mutex_t));
        pthread_mutex_init(m, NULL);
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
 * ========================================================================= */

static pthread_mutex_t g_atomic_lock = PTHREAD_MUTEX_INITIALIZER;

void GOMP_atomic_start(void) { pthread_mutex_lock(&g_atomic_lock); }
void GOMP_atomic_end(void) { pthread_mutex_unlock(&g_atomic_lock); }

/* =========================================================================
 * GOMP_single_start
 * ========================================================================= */

bool GOMP_single_start(void) {
    int my_gen = tl_single_generation;
    tl_single_generation++;
    return __sync_bool_compare_and_swap(&g_single_generation, my_gen, my_gen + 1);
}

void *GOMP_single_copy_start(void) {
    if (tl_thread_num == 0) return NULL;
    return NULL;
}

void GOMP_single_copy_end(void *data) { (void)data; }

/* =========================================================================
 * GOMP_ordered
 * ========================================================================= */

static pthread_mutex_t g_ordered_lock = PTHREAD_MUTEX_INITIALIZER;

void GOMP_ordered_start(void) { pthread_mutex_lock(&g_ordered_lock); }
void GOMP_ordered_end(void) { pthread_mutex_unlock(&g_ordered_lock); }

/* =========================================================================
 * GOMP_loop_* — worksharing loops
 * ========================================================================= */

/* Forward declaration */
bool GOMP_loop_dynamic_next(long *istart, long *iend);

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

    if (chunk_size <= 0) chunk_size = (total + nthreads - 1) / nthreads;

    long my_start_iter = tid * chunk_size;
    if (my_start_iter >= total) return false;

    long my_end_iter = my_start_iter + chunk_size;
    if (my_end_iter > total) my_end_iter = total;

    *istart = start + my_start_iter * incr;
    *iend = start + my_end_iter * incr;

    tl_loop_start = start;
    tl_loop_end = end;
    tl_loop_incr = incr;
    tl_loop_chunk = chunk_size;
    tl_loop_cur = my_end_iter;
    tl_loop_own_end = total;

    return true;
}

bool GOMP_loop_static_next(long *istart, long *iend) {
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

    if (tl_thread_num == 0) {
        g_dynamic_next_iter = 0;
        g_dynamic_total = total;
        g_dynamic_start = start;
        g_dynamic_incr = incr;
        g_dynamic_chunk = chunk_size;
    }
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

bool GOMP_loop_guided_start(long start, long end, long incr,
                            long chunk_size, long *istart, long *iend) {
    return GOMP_loop_dynamic_start(start, end, incr, chunk_size, istart, iend);
}

bool GOMP_loop_guided_next(long *istart, long *iend) {
    return GOMP_loop_dynamic_next(istart, iend);
}

bool GOMP_loop_runtime_start(long start, long end, long incr,
                             long chunk_size, long *istart, long *iend) {
    return GOMP_loop_static_start(start, end, incr, chunk_size, istart, iend);
}

bool GOMP_loop_runtime_next(long *istart, long *iend) {
    return GOMP_loop_static_next(istart, iend);
}

void GOMP_loop_end(void) { GOMP_barrier(); }
void GOMP_loop_end_nowait(void) { }

bool GOMP_loop_start(long start, long end, long incr, long chunk_size,
                     long *istart, long *iend, void **reductions, void **mem) {
    (void)reductions; (void)mem;
    return GOMP_loop_static_start(start, end, incr, chunk_size, istart, iend);
}

void GOMP_parallel_loop_static(void (*fn)(void *), void *data,
                               unsigned num_threads, long start, long end,
                               long incr, long chunk_size, unsigned flags) {
    (void)start; (void)end; (void)incr; (void)chunk_size;
    GOMP_parallel(fn, data, num_threads, flags);
}

void GOMP_parallel_loop_dynamic(void (*fn)(void *), void *data,
                                unsigned num_threads, long start, long end,
                                long incr, long chunk_size, unsigned flags) {
    (void)start; (void)end; (void)incr; (void)chunk_size;
    GOMP_parallel(fn, data, num_threads, flags);
}

void GOMP_parallel_loop_guided(void (*fn)(void *), void *data,
                               unsigned num_threads, long start, long end,
                               long incr, long chunk_size, unsigned flags) {
    (void)start; (void)end; (void)incr; (void)chunk_size;
    GOMP_parallel(fn, data, num_threads, flags);
}

void GOMP_parallel_loop_runtime(void (*fn)(void *), void *data,
                                unsigned num_threads, long start, long end,
                                long incr, long chunk_size, unsigned flags) {
    (void)start; (void)end; (void)incr; (void)chunk_size;
    GOMP_parallel(fn, data, num_threads, flags);
}

/* =========================================================================
 * GOMP_task / GOMP_taskwait — inline execution
 * ========================================================================= */

void GOMP_task(void (*fn)(void *), void *data,
               void (*cpyfn)(void *, void *),
               long arg_size, long arg_align,
               bool if_clause, unsigned flags,
               void **depend, int priority) {
    (void)arg_align; (void)flags; (void)depend; (void)priority;
    if (if_clause && cpyfn) {
        void *buf = malloc(arg_size);
        cpyfn(buf, data);
        fn(buf);
        free(buf);
    } else {
        fn(data);
    }
}

void GOMP_taskwait(void) { }
void GOMP_taskyield(void) { }
void GOMP_taskgroup_start(void) { }
void GOMP_taskgroup_end(void) { }

/* =========================================================================
 * GOMP_sections
 * ========================================================================= */

static int g_sections_next = 0;
static int g_sections_count = 0;

unsigned GOMP_sections_start(unsigned count) {
    g_sections_count = count;
    g_sections_next = 1;
    return 0;
}

unsigned GOMP_sections_next(void) {
    int s = __sync_fetch_and_add(&g_sections_next, 1);
    if (s >= g_sections_count) return 0;
    return s;
}

void GOMP_sections_end(void) { GOMP_barrier(); }
void GOMP_sections_end_nowait(void) { }

void GOMP_parallel_sections(void (*fn)(void *), void *data,
                            unsigned num_threads, unsigned count,
                            unsigned flags) {
    (void)count;
    GOMP_parallel(fn, data, num_threads, flags);
}

/* =========================================================================
 * omp_* user API
 * ========================================================================= */

int omp_get_num_threads(void) { return tl_num_threads; }
int omp_get_thread_num(void) { return tl_thread_num; }

int omp_get_max_threads(void) {
    ensure_rts();
    return g_rts_num_caps;
}

int omp_get_num_procs(void) {
    return (int)getNumberOfProcessors();
}

void omp_set_num_threads(int n) {
    if (n > 0) g_num_threads = n;
}

int omp_in_parallel(void) { return tl_in_parallel ? 1 : 0; }
void omp_set_dynamic(int val) { (void)val; }
int omp_get_dynamic(void) { return 0; }
void omp_set_nested(int val) { (void)val; }
int omp_get_nested(void) { return 0; }

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

/* Locks */
typedef pthread_mutex_t omp_lock_t;

void omp_init_lock(omp_lock_t *lock) { pthread_mutex_init(lock, NULL); }
void omp_destroy_lock(omp_lock_t *lock) { pthread_mutex_destroy(lock); }
void omp_set_lock(omp_lock_t *lock) { pthread_mutex_lock(lock); }
void omp_unset_lock(omp_lock_t *lock) { pthread_mutex_unlock(lock); }
int omp_test_lock(omp_lock_t *lock) { return pthread_mutex_trylock(lock) == 0; }

typedef pthread_mutex_t omp_nest_lock_t;

void omp_init_nest_lock(omp_nest_lock_t *lock) {
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(lock, &attr);
    pthread_mutexattr_destroy(&attr);
}

void omp_destroy_nest_lock(omp_nest_lock_t *lock) { pthread_mutex_destroy(lock); }
void omp_set_nest_lock(omp_nest_lock_t *lock) { pthread_mutex_lock(lock); }
void omp_unset_nest_lock(omp_nest_lock_t *lock) { pthread_mutex_unlock(lock); }
int omp_test_nest_lock(omp_nest_lock_t *lock) { return pthread_mutex_trylock(lock) == 0; }

/* OpenMP 3.0+ */
int omp_get_level(void) { return tl_in_parallel ? 1 : 0; }
int omp_get_active_level(void) { return tl_in_parallel ? 1 : 0; }
int omp_get_ancestor_thread_num(int level) { return (level == 1) ? tl_thread_num : 0; }
int omp_get_team_size(int level) { return (level == 1) ? tl_num_threads : 1; }
int omp_get_thread_limit(void) { return GHC_OMP_MAX_THREADS; }
void omp_set_max_active_levels(int val) { (void)val; }
int omp_get_max_active_levels(void) { return 1; }
int omp_get_supported_active_levels(void) { return 1; }

typedef enum { omp_sched_static=1, omp_sched_dynamic=2,
               omp_sched_guided=3, omp_sched_auto=4 } omp_sched_t;
static omp_sched_t g_schedule = omp_sched_static;
static int g_schedule_chunk = 0;

void omp_set_schedule(omp_sched_t kind, int chunk) { g_schedule = kind; g_schedule_chunk = chunk; }
void omp_get_schedule(omp_sched_t *kind, int *chunk) { *kind = g_schedule; *chunk = g_schedule_chunk; }

int omp_in_final(void) { return 0; }
int omp_get_cancellation(void) { return 0; }
int omp_get_proc_bind(void) { return 0; }
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

/* Cancel stubs */
void GOMP_cancel(int which, bool do_cancel) { (void)which; (void)do_cancel; }
void GOMP_cancellation_point(int which) { (void)which; }
void GOMP_barrier_cancel(void) {}
void GOMP_loop_end_cancel(void) {}
void GOMP_sections_end_cancel(void) {}

/* Teams stubs */
void GOMP_teams_reg(void (*fn)(void *), void *data, unsigned num_teams,
                    unsigned thread_limit, unsigned flags) {
    (void)num_teams; (void)thread_limit; (void)flags;
    fn(data);
}
