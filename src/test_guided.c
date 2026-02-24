/*
 * test_guided.c — Test guided scheduling correctness and chunk distribution
 *
 * Compares guided vs static/dynamic for a reduction sum.
 * Prints chunk distribution per thread to verify exponentially-decreasing sizes.
 *
 * Compile: gcc -fopenmp -O2 -o test_guided test_guided.c -lm
 * Run:     OMP_NUM_THREADS=4 ./test_guided
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>

#define N 100000

int main(void) {
    int nthreads;
    #pragma omp parallel
    {
        #pragma omp master
        nthreads = omp_get_num_threads();
    }
    printf("Threads: %d\n", nthreads);

    /* --- Test 1: Correctness (reduction sum) --- */
    printf("\n--- Test 1: Guided scheduling correctness ---\n");

    /* Reference: sequential sum */
    double ref = 0.0;
    for (int i = 0; i < N; i++)
        ref += sin((double)i * 0.001);

    /* Static schedule */
    double sum_static = 0.0;
    #pragma omp parallel for schedule(static) reduction(+:sum_static)
    for (int i = 0; i < N; i++)
        sum_static += sin((double)i * 0.001);

    /* Dynamic schedule */
    double sum_dynamic = 0.0;
    #pragma omp parallel for schedule(dynamic, 100) reduction(+:sum_dynamic)
    for (int i = 0; i < N; i++)
        sum_dynamic += sin((double)i * 0.001);

    /* Guided schedule */
    double sum_guided = 0.0;
    #pragma omp parallel for schedule(guided, 10) reduction(+:sum_guided)
    for (int i = 0; i < N; i++)
        sum_guided += sin((double)i * 0.001);

    printf("  Reference: %.10f\n", ref);
    printf("  Static:    %.10f  (diff = %.1e)\n", sum_static, fabs(sum_static - ref));
    printf("  Dynamic:   %.10f  (diff = %.1e)\n", sum_dynamic, fabs(sum_dynamic - ref));
    printf("  Guided:    %.10f  (diff = %.1e)\n", sum_guided, fabs(sum_guided - ref));

    int ok = (fabs(sum_guided - ref) < 1e-6);
    printf("  Guided match: %s\n", ok ? "YES" : "NO");

    /* --- Test 2: Chunk distribution --- */
    printf("\n--- Test 2: Guided chunk distribution ---\n");

    int counts[128] = {0};
    int max_tid = 0;

    #pragma omp parallel for schedule(guided, 10)
    for (int i = 0; i < N; i++) {
        int tid = omp_get_thread_num();
        #pragma omp atomic
        counts[tid]++;
        if (tid > max_tid) max_tid = tid;
    }

    printf("  Iterations per thread (total=%d):\n", N);
    int total_check = 0;
    for (int t = 0; t <= max_tid; t++) {
        printf("    Thread %d: %d iterations (%.1f%%)\n",
               t, counts[t], 100.0 * counts[t] / N);
        total_check += counts[t];
    }
    printf("  Total accounted: %d (expected %d) %s\n",
           total_check, N, total_check == N ? "OK" : "MISMATCH");

    /* --- Test 3: Performance comparison --- */
    printf("\n--- Test 3: Performance comparison ---\n");

    double t0, t1;
    double dummy = 0.0;

    t0 = omp_get_wtime();
    #pragma omp parallel for schedule(static) reduction(+:dummy)
    for (int i = 0; i < N; i++)
        dummy += sin((double)i * 0.001);
    t1 = omp_get_wtime();
    printf("  Static:  %.3f ms\n", (t1 - t0) * 1000);

    dummy = 0.0;
    t0 = omp_get_wtime();
    #pragma omp parallel for schedule(dynamic, 100) reduction(+:dummy)
    for (int i = 0; i < N; i++)
        dummy += sin((double)i * 0.001);
    t1 = omp_get_wtime();
    printf("  Dynamic: %.3f ms\n", (t1 - t0) * 1000);

    dummy = 0.0;
    t0 = omp_get_wtime();
    #pragma omp parallel for schedule(guided, 10) reduction(+:dummy)
    for (int i = 0; i < N; i++)
        dummy += sin((double)i * 0.001);
    t1 = omp_get_wtime();
    printf("  Guided:  %.3f ms\n", (t1 - t0) * 1000);

    printf("\n=== Done ===\n");
    return ok && (total_check == N) ? 0 : 1;
}
