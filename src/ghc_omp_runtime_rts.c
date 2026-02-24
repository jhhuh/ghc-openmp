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
#include <sched.h>
#include <string.h>

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
#define SPIN_ITERS_DEFAULT 4000  /* spin iterations before falling back */

static int g_spin_iters = SPIN_ITERS_DEFAULT;

/* =========================================================================
 * Sense-reversing centralized barrier (lock-free)
 *
 * Algorithm from Mellor-Crummey & Scott, "Algorithms for Scalable
 * Synchronization on Shared-Memory Multiprocessors", ACM TOCS 9(1), 1991.
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
        /* Hybrid spin-then-yield until sense flips */
        int spins = 0;
        while (atomic_load_explicit(&b->sense, memory_order_acquire) != *local_sense) {
            if (++spins < g_spin_iters)
                __builtin_ia32_pause();
            else
                sched_yield();
        }
    }
}

/* Task queue state — declared early so barriers can steal tasks */
static atomic_int g_tasks_pending = 0;
static bool task_try_steal_one(void);

/* Forward declarations for init_rts() */
static void task_pool_init(void);
static void task_queues_init(void);

/* Barrier with integrated task stealing.
 * While spinning for other threads, steal and execute queued tasks.
 * Last thread arriving drains remaining tasks before releasing. */
static inline void spin_barrier_wait_tasks(spin_barrier_t *b, int *local_sense) {
    *local_sense = 1 - *local_sense;
    if (atomic_fetch_sub_explicit(&b->count, 1, memory_order_acq_rel) == 1) {
        /* Last thread: drain remaining tasks before releasing */
        while (atomic_load_explicit(&g_tasks_pending, memory_order_acquire) > 0) {
            if (!task_try_steal_one())
                __builtin_ia32_pause();
        }
        atomic_store_explicit(&b->count, b->size, memory_order_relaxed);
        atomic_store_explicit(&b->sense, *local_sense, memory_order_release);
    } else {
        /* Hybrid spin-then-yield — steal tasks while waiting */
        int spins = 0;
        while (atomic_load_explicit(&b->sense, memory_order_acquire) != *local_sense) {
            if (task_try_steal_one()) {
                spins = 0; /* reset after productive work */
            } else if (++spins < g_spin_iters) {
                __builtin_ia32_pause();
            } else {
                sched_yield();
            }
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

/* Nested parallelism state */
#define OMP_MAX_NESTING 8
typedef struct {
    int thread_num;
    int num_threads;
} nesting_level_t;

static __thread int tl_nesting_level = 0;
static __thread nesting_level_t tl_level_state[OMP_MAX_NESTING];
static bool g_nested_enabled = false;
static int g_max_active_levels = 1;

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
            if (++spins < g_spin_iters) {
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
            tl_nesting_level = 1;
            tl_level_state[0].thread_num = 0;
            tl_level_state[0].num_threads = 1;
            tl_level_state[1].thread_num = worker_id;
            tl_level_state[1].num_threads = num_threads;

            /* Sync: all team members ready */
            spin_barrier_wait(&g_pool.start_barrier, &start_sense);

            /* Execute */
            fn(data);

            /* End barrier — steal pending tasks while waiting */
            spin_barrier_wait_tasks(&g_pool.end_barrier, &end_sense);

            tl_in_parallel = false;
            tl_nesting_level = 0;

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
    /* Parse OMP_WAIT_POLICY */
    const char *wait_policy = getenv("OMP_WAIT_POLICY");
    if (wait_policy) {
        if (strcasecmp(wait_policy, "passive") == 0)
            g_spin_iters = 100;
        else if (strcasecmp(wait_policy, "active") == 0)
            g_spin_iters = 10000;
    }

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

    /* Initialize task pool and per-Capability queues */
    task_pool_init();
    task_queues_init();

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

    /* Nested parallelism: serialize inner regions */
    if (tl_in_parallel) {
        DBG("GOMP_parallel: nested region, serializing");
        /* Save current nesting state */
        int saved_thread_num = tl_thread_num;
        int saved_num_threads = tl_num_threads;
        int saved_level = tl_nesting_level;

        /* Push nesting level */
        int level = tl_nesting_level + 1;
        if (level < OMP_MAX_NESTING) {
            tl_level_state[level].thread_num = 0;
            tl_level_state[level].num_threads = 1;
        }
        tl_nesting_level = level;
        tl_thread_num = 0;
        tl_num_threads = 1;

        fn(data);

        /* Restore */
        tl_nesting_level = saved_level;
        tl_thread_num = saved_thread_num;
        tl_num_threads = saved_num_threads;
        return;
    }

    int nthreads = resolve_num_threads(num_threads);
    if (nthreads < 1) nthreads = 1;
    if (nthreads > g_rts_num_caps) nthreads = g_rts_num_caps;

    DBG("GOMP_parallel: nthreads=%d, pool_size=%d", nthreads, g_pool_size);

    /* Save/set master thread-local state */
    int saved_thread_num = tl_thread_num;
    int saved_num_threads = tl_num_threads;
    bool saved_in_parallel = tl_in_parallel;
    int saved_level = tl_nesting_level;

    /* Set nesting level 1 state */
    tl_nesting_level = 1;
    tl_level_state[0].thread_num = 0;
    tl_level_state[0].num_threads = 1;
    tl_level_state[1].thread_num = 0;
    tl_level_state[1].num_threads = nthreads;

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

        /* End barrier — steal pending tasks while waiting */
        spin_barrier_wait_tasks(&g_pool.end_barrier, &master_end_sense);

        /* Spin-wait for all workers to signal done */
        while (atomic_load_explicit(&g_pool.workers_done,
                                    memory_order_acquire) < num_workers) {
            __builtin_ia32_pause();
        }
    }

    /* Restore */
    tl_nesting_level = saved_level;
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
    spin_barrier_wait_tasks(&g_omp_barrier, &tl_barrier_sense);
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

/* Forward declarations */
bool GOMP_loop_dynamic_next(long *istart, long *iend);
bool GOMP_loop_guided_next(long *istart, long *iend);

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

/* Guided scheduling: exponentially-decreasing chunk sizes via CAS */
static _Atomic long g_guided_remaining = 0;
static long g_guided_total = 0;
static long g_guided_start_val = 0;
static long g_guided_incr = 1;
static long g_guided_min_chunk = 1;

bool GOMP_loop_guided_start(long start, long end, long incr,
                            long chunk_size, long *istart, long *iend) {
    long total = (end - start + incr - (incr > 0 ? 1 : -1)) / incr;
    if (total <= 0) return false;
    if (chunk_size <= 0) chunk_size = 1;

    if (tl_thread_num == 0) {
        g_guided_total = total;
        g_guided_start_val = start;
        g_guided_incr = incr;
        g_guided_min_chunk = chunk_size;
        atomic_store_explicit(&g_guided_remaining, total,
                              memory_order_relaxed);
    }
    GOMP_barrier();
    return GOMP_loop_guided_next(istart, iend);
}

bool GOMP_loop_guided_next(long *istart, long *iend) {
    int nthreads = tl_num_threads;
    long min_chunk = g_guided_min_chunk;
    long total = g_guided_total;

    while (1) {
        long rem = atomic_load_explicit(&g_guided_remaining,
                                        memory_order_acquire);
        if (rem <= 0) return false;

        /* Guided: chunk = max(min_chunk, remaining / nthreads) */
        long chunk = rem / nthreads;
        if (chunk < min_chunk) chunk = min_chunk;
        if (chunk > rem) chunk = rem;

        long new_rem = rem - chunk;
        if (atomic_compare_exchange_weak_explicit(&g_guided_remaining,
                                                   &rem, new_rem,
                                                   memory_order_acq_rel,
                                                   memory_order_acquire)) {
            /* Claimed [total-rem, total-rem+chunk) */
            long iter_start = total - rem;
            long iter_end = iter_start + chunk;
            *istart = g_guided_start_val + iter_start * g_guided_incr;
            *iend = g_guided_start_val + iter_end * g_guided_incr;
            return true;
        }
        /* CAS failed, retry with updated rem */
    }
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
    long total = (end - start + incr - (incr > 0 ? 1 : -1)) / incr;
    if (chunk_size <= 0) chunk_size = 1;
    g_dynamic_next_iter = 0;
    g_dynamic_total = total > 0 ? total : 0;
    g_dynamic_start = start;
    g_dynamic_incr = incr;
    g_dynamic_chunk = chunk_size;
    GOMP_parallel(fn, data, num_threads, flags);
}

void GOMP_parallel_loop_guided(void (*fn)(void *), void *data,
                               unsigned num_threads, long start, long end,
                               long incr, long chunk_size, unsigned flags) {
    long total = (end - start + incr - (incr > 0 ? 1 : -1)) / incr;
    if (chunk_size <= 0) chunk_size = 1;
    g_guided_total = total > 0 ? total : 0;
    g_guided_start_val = start;
    g_guided_incr = incr;
    g_guided_min_chunk = chunk_size;
    atomic_store_explicit(&g_guided_remaining, total > 0 ? total : 0,
                          memory_order_relaxed);
    GOMP_parallel(fn, data, num_threads, flags);
}

void GOMP_parallel_loop_runtime(void (*fn)(void *), void *data,
                                unsigned num_threads, long start, long end,
                                long incr, long chunk_size, unsigned flags) {
    (void)start; (void)end; (void)incr; (void)chunk_size;
    GOMP_parallel(fn, data, num_threads, flags);
}

/* Nonmonotonic variants (GCC 9+) — identical semantics for our runtime */
bool GOMP_loop_nonmonotonic_dynamic_start(long start, long end, long incr,
                                           long chunk_size, long *istart, long *iend) {
    return GOMP_loop_dynamic_start(start, end, incr, chunk_size, istart, iend);
}
bool GOMP_loop_nonmonotonic_dynamic_next(long *istart, long *iend) {
    return GOMP_loop_dynamic_next(istart, iend);
}
bool GOMP_loop_nonmonotonic_guided_start(long start, long end, long incr,
                                          long chunk_size, long *istart, long *iend) {
    return GOMP_loop_guided_start(start, end, incr, chunk_size, istart, iend);
}
bool GOMP_loop_nonmonotonic_guided_next(long *istart, long *iend) {
    return GOMP_loop_guided_next(istart, iend);
}
void GOMP_parallel_loop_nonmonotonic_dynamic(void (*fn)(void *), void *data,
                                              unsigned num_threads, long start, long end,
                                              long incr, long chunk_size, unsigned flags) {
    GOMP_parallel_loop_dynamic(fn, data, num_threads, start, end, incr,
                               chunk_size, flags);
}
void GOMP_parallel_loop_nonmonotonic_guided(void (*fn)(void *), void *data,
                                             unsigned num_threads, long start, long end,
                                             long incr, long chunk_size, unsigned flags) {
    GOMP_parallel_loop_guided(fn, data, num_threads, start, end, incr,
                              chunk_size, flags);
}

/* =========================================================================
 * GOMP_task / GOMP_taskwait — deferred execution with task stealing
 *
 * Pre-allocated task descriptor pool with per-Capability lock-free queues.
 * Each thread pushes to its own queue; idle threads steal from others.
 * ========================================================================= */

typedef struct task_entry {
    void (*fn)(void *);
    void *data;
    struct task_entry *next;
} task_entry_t;

/* --- Pre-allocated task pool --- */
#define TASK_POOL_SIZE 4096

static task_entry_t g_task_pool[TASK_POOL_SIZE];
static _Atomic(task_entry_t *) g_task_freelist = NULL;         /* global overflow */
static _Atomic(task_entry_t *) g_task_freelists[GHC_OMP_MAX_THREADS]; /* per-cap */

static void task_pool_init(void) {
    /* Chain all entries into global free list */
    for (int i = TASK_POOL_SIZE - 1; i >= 0; i--) {
        g_task_pool[i].next = (i < TASK_POOL_SIZE - 1) ?
            atomic_load(&g_task_freelist) : NULL;
        atomic_store(&g_task_freelist, &g_task_pool[i]);
    }
}

static inline bool task_is_pooled(task_entry_t *e) {
    return e >= &g_task_pool[0] && e < &g_task_pool[TASK_POOL_SIZE];
}

static task_entry_t *task_alloc(void) {
    int tid = tl_thread_num;

    /* Try per-cap free list first */
    task_entry_t *e = atomic_load_explicit(&g_task_freelists[tid],
                                           memory_order_acquire);
    while (e) {
        if (atomic_compare_exchange_weak_explicit(&g_task_freelists[tid],
                                                   &e, e->next,
                                                   memory_order_acq_rel,
                                                   memory_order_acquire))
            return e;
    }

    /* Try global free list */
    e = atomic_load_explicit(&g_task_freelist, memory_order_acquire);
    while (e) {
        if (atomic_compare_exchange_weak_explicit(&g_task_freelist,
                                                   &e, e->next,
                                                   memory_order_acq_rel,
                                                   memory_order_acquire))
            return e;
    }

    /* Fallback to malloc */
    return malloc(sizeof(task_entry_t));
}

static void task_free(task_entry_t *e) {
    if (task_is_pooled(e)) {
        /* Push to local free list */
        int tid = tl_thread_num;
        task_entry_t *head;
        do {
            head = atomic_load_explicit(&g_task_freelists[tid],
                                        memory_order_acquire);
            e->next = head;
        } while (!atomic_compare_exchange_weak_explicit(
            &g_task_freelists[tid], &head, e,
            memory_order_acq_rel, memory_order_acquire));
    } else {
        free(e);
    }
}

/* --- Per-Capability task queues with spinlocks --- */
typedef struct {
    task_entry_t *head;
    task_entry_t *tail;
    atomic_flag lock;
    char _pad[64 - sizeof(task_entry_t *) * 2 - sizeof(atomic_flag)];
} task_queue_t;

static task_queue_t g_task_queues[GHC_OMP_MAX_THREADS];

static void task_queues_init(void) {
    for (int i = 0; i < GHC_OMP_MAX_THREADS; i++) {
        g_task_queues[i].head = NULL;
        g_task_queues[i].tail = NULL;
        atomic_flag_clear(&g_task_queues[i].lock);
    }
}

static inline void tq_lock(task_queue_t *q) {
    while (atomic_flag_test_and_set_explicit(&q->lock, memory_order_acquire))
        __builtin_ia32_pause();
}

static inline void tq_unlock(task_queue_t *q) {
    atomic_flag_clear_explicit(&q->lock, memory_order_release);
}

static void task_queue_push(void (*fn)(void *), void *data) {
    task_entry_t *entry = task_alloc();
    entry->fn = fn;
    entry->data = data;
    entry->next = NULL;

    int tid = tl_thread_num;
    task_queue_t *q = &g_task_queues[tid];
    tq_lock(q);
    if (q->tail) {
        q->tail->next = entry;
    } else {
        q->head = entry;
    }
    q->tail = entry;
    tq_unlock(q);

    atomic_fetch_add_explicit(&g_tasks_pending, 1, memory_order_release);
}

/* Pop from own queue */
static task_entry_t *task_queue_try_pop_local(void) {
    int tid = tl_thread_num;
    task_queue_t *q = &g_task_queues[tid];
    task_entry_t *entry = NULL;

    tq_lock(q);
    if (q->head) {
        entry = q->head;
        q->head = entry->next;
        if (!q->head) q->tail = NULL;
    }
    tq_unlock(q);
    return entry;
}

/* Steal from another thread's queue */
static task_entry_t *task_queue_steal(void) {
    int tid = tl_thread_num;
    int nthreads = tl_num_threads;
    /* Linear scan from pseudo-random start, skip self */
    unsigned start = (unsigned)tid * 2654435761u; /* Knuth multiplicative hash */
    for (int i = 0; i < nthreads; i++) {
        int victim = (int)((start + (unsigned)i) % (unsigned)nthreads);
        if (victim == tid) continue;

        task_queue_t *q = &g_task_queues[victim];
        task_entry_t *entry = NULL;
        tq_lock(q);
        if (q->head) {
            entry = q->head;
            q->head = entry->next;
            if (!q->head) q->tail = NULL;
        }
        tq_unlock(q);
        if (entry) return entry;
    }
    return NULL;
}

/* Execute tasks from all queues until all pending tasks are complete. */
static void task_drain_queue(void) {
    while (atomic_load_explicit(&g_tasks_pending, memory_order_acquire) > 0) {
        task_entry_t *entry = task_queue_try_pop_local();
        if (!entry) entry = task_queue_steal();
        if (entry) {
            entry->fn(entry->data);
            free(entry->data);
            task_free(entry);
            atomic_fetch_sub_explicit(&g_tasks_pending, 1,
                                      memory_order_release);
        } else {
            __builtin_ia32_pause();
        }
    }
}

/* Try to steal one task. Returns true if a task was executed. */
static bool task_try_steal_one(void) {
    task_entry_t *entry = task_queue_try_pop_local();
    if (!entry) entry = task_queue_steal();
    if (entry) {
        entry->fn(entry->data);
        free(entry->data);
        task_free(entry);
        atomic_fetch_sub_explicit(&g_tasks_pending, 1,
                                  memory_order_release);
        return true;
    }
    return false;
}

void GOMP_task(void (*fn)(void *), void *data,
               void (*cpyfn)(void *, void *),
               long arg_size, long arg_align,
               bool if_clause, unsigned flags,
               void **depend, int priority) {
    (void)flags; (void)depend; (void)priority;

    if (!if_clause || !tl_in_parallel) {
        /* Execute immediately:
         * - if_clause is false (OpenMP spec says must execute inline)
         * - Not in parallel region: no other threads to steal */
        if (cpyfn) {
            long align = arg_align > 0 ? arg_align : 8;
            long sz = ((arg_size + align - 1) / align) * align;
            void *buf = aligned_alloc(align, sz);
            cpyfn(buf, data);
            fn(buf);
            free(buf);
        } else {
            fn(data);
        }
    } else {
        /* Defer: copy data to heap (via cpyfn or memcpy), push to task queue */
        long align = arg_align > 0 ? arg_align : 8;
        long sz = ((arg_size + align - 1) / align) * align;
        void *buf = aligned_alloc(align, sz > 0 ? sz : 8);
        if (cpyfn) {
            cpyfn(buf, data);
        } else if (arg_size > 0) {
            __builtin_memcpy(buf, data, arg_size);
        }
        task_queue_push(fn, buf);
    }
}

void GOMP_taskwait(void) {
    task_drain_queue();
}
void GOMP_taskyield(void) {
    /* Try to make progress on pending tasks */
    task_try_steal_one();
}
void GOMP_taskgroup_start(void) { }
void GOMP_taskgroup_end(void) {
    /* Wait for all tasks in this group */
    task_drain_queue();
}

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
void omp_set_nested(int val) { g_nested_enabled = (val != 0); }
int omp_get_nested(void) { return g_nested_enabled ? 1 : 0; }

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

/* OpenMP 3.0+ nesting support */
int omp_get_level(void) { return tl_nesting_level; }

int omp_get_active_level(void) {
    int active = 0;
    for (int i = 1; i <= tl_nesting_level && i < OMP_MAX_NESTING; i++) {
        if (tl_level_state[i].num_threads > 1) active++;
    }
    return active;
}

int omp_get_ancestor_thread_num(int level) {
    if (level < 0 || level > tl_nesting_level) return -1;
    if (level == 0) return 0;
    if (level < OMP_MAX_NESTING) return tl_level_state[level].thread_num;
    return -1;
}

int omp_get_team_size(int level) {
    if (level < 0 || level > tl_nesting_level) return -1;
    if (level == 0) return 1;
    if (level < OMP_MAX_NESTING) return tl_level_state[level].num_threads;
    return -1;
}

int omp_get_thread_limit(void) { return GHC_OMP_MAX_THREADS; }

void omp_set_max_active_levels(int val) {
    if (val >= 0) g_max_active_levels = val;
}
int omp_get_max_active_levels(void) { return g_max_active_levels; }
int omp_get_supported_active_levels(void) { return OMP_MAX_NESTING; }

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
