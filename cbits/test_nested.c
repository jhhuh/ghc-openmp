/*
 * test_nested.c — Test nested parallel region level/team-size queries
 *
 * Verifies omp_get_level(), omp_get_active_level(),
 * omp_get_ancestor_thread_num(), omp_get_team_size() across nesting.
 *
 * Compile: gcc -fopenmp -O2 -o test_nested test_nested.c
 * Run:     OMP_NUM_THREADS=4 ./test_nested
 */

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

static int g_errors = 0;

#define CHECK(cond, fmt, ...) do { \
    if (!(cond)) { \
        printf("  FAIL: " fmt "\n", ##__VA_ARGS__); \
        __atomic_fetch_add(&g_errors, 1, __ATOMIC_RELAXED); \
    } \
} while(0)

int main(void) {
    int nthreads;
    #pragma omp parallel
    {
        #pragma omp master
        nthreads = omp_get_num_threads();
    }
    printf("Threads: %d\n", nthreads);

    /* --- Test 1: Level tracking at top level --- */
    printf("\n--- Test 1: Level tracking at top level ---\n");

    printf("  Outside parallel: level=%d, active_level=%d\n",
           omp_get_level(), omp_get_active_level());
    CHECK(omp_get_level() == 0, "expected level=0 outside parallel, got %d",
          omp_get_level());

    /* --- Test 2: Level tracking inside parallel --- */
    printf("\n--- Test 2: Level tracking inside parallel ---\n");

    #pragma omp parallel
    {
        #pragma omp master
        {
            int lev = omp_get_level();
            int alev = omp_get_active_level();
            int anc0 = omp_get_ancestor_thread_num(0);
            int anc1 = omp_get_ancestor_thread_num(1);
            int ts0 = omp_get_team_size(0);
            int ts1 = omp_get_team_size(1);
            printf("  Level 1: level=%d, active_level=%d\n", lev, alev);
            printf("  ancestor(0)=%d, ancestor(1)=%d\n", anc0, anc1);
            printf("  team_size(0)=%d, team_size(1)=%d\n", ts0, ts1);

            CHECK(lev == 1, "expected level=1, got %d", lev);
            CHECK(alev == 1, "expected active_level=1, got %d", alev);
            CHECK(anc0 == 0, "expected ancestor(0)=0, got %d", anc0);
            CHECK(anc1 == 0, "expected ancestor(1)=0 (master), got %d", anc1);
            CHECK(ts0 == 1, "expected team_size(0)=1, got %d", ts0);
            CHECK(ts1 == nthreads, "expected team_size(1)=%d, got %d",
                  nthreads, ts1);
        }
    }

    /* --- Test 3: Nested parallel (serialized) --- */
    printf("\n--- Test 3: Nested parallel (serialized) ---\n");

    #pragma omp parallel num_threads(2)
    {
        #pragma omp master
        {
            int outer_level = omp_get_level();
            int outer_nt = omp_get_num_threads();
            printf("  Outer: level=%d, num_threads=%d\n",
                   outer_level, outer_nt);

            /* Nested region — should be serialized */
            #pragma omp parallel num_threads(4)
            {
                int inner_level = omp_get_level();
                int inner_nt = omp_get_num_threads();
                int inner_tid = omp_get_thread_num();

                /* Only one thread should execute (serialized) */
                printf("  Inner: level=%d, num_threads=%d, tid=%d\n",
                       inner_level, inner_nt, inner_tid);

                CHECK(inner_level == 2,
                      "expected inner level=2, got %d", inner_level);
                CHECK(inner_nt == 1,
                      "expected inner num_threads=1 (serialized), got %d",
                      inner_nt);
                CHECK(inner_tid == 0,
                      "expected inner tid=0, got %d", inner_tid);

                /* Check ancestor queries from inner level */
                int ts1 = omp_get_team_size(1);
                int ts2 = omp_get_team_size(2);
                printf("  Inner: team_size(1)=%d, team_size(2)=%d\n",
                       ts1, ts2);
                CHECK(ts2 == 1,
                      "expected team_size(2)=1, got %d", ts2);
            }
        }
    }

    /* --- Test 4: All threads get correct level --- */
    printf("\n--- Test 4: Per-thread level check ---\n");

    int level_ok = 1;
    #pragma omp parallel
    {
        int lev = omp_get_level();
        int tid = omp_get_thread_num();
        int anc = omp_get_ancestor_thread_num(1);
        if (lev != 1 || anc != tid) {
            printf("  Thread %d: level=%d (expected 1), ancestor(1)=%d (expected %d)\n",
                   tid, lev, anc, tid);
            __atomic_fetch_add(&level_ok, -1, __ATOMIC_RELAXED);
        }
    }
    printf("  All threads level=1: %s\n", level_ok > 0 ? "YES" : "NO");
    CHECK(level_ok > 0, "some threads had wrong level or ancestor");

    /* --- Test 5: Level restored after parallel --- */
    printf("\n--- Test 5: Level restored after parallel ---\n");
    int post_level = omp_get_level();
    printf("  After parallel: level=%d\n", post_level);
    CHECK(post_level == 0, "expected level=0 after parallel, got %d", post_level);

    /* --- Summary --- */
    printf("\n=== %s (%d errors) ===\n",
           g_errors == 0 ? "PASSED" : "FAILED", g_errors);
    return g_errors == 0 ? 0 : 1;
}
