/*
 * test_omp_tasks.c — Test deferred OpenMP task execution
 *
 * Creates many tasks inside a parallel single block. With deferred
 * execution, idle threads steal tasks from the queue instead of waiting.
 *
 * Compile: gcc -fopenmp -O2 -o test_tasks test_omp_tasks.c -lm
 * Run:     OMP_NUM_THREADS=4 ./test_tasks
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>

#define NTASKS 10000
#define WORK_PER_TASK 1000

static double results[NTASKS];

int main(void) {
    int nthreads;
    #pragma omp parallel
    {
        #pragma omp master
        nthreads = omp_get_num_threads();
    }
    printf("Threads: %d\n", nthreads);

    /* --- Test 1: Correctness --- */
    printf("\n--- Test 1: Correctness (%d tasks, %d work/task) ---\n",
           NTASKS, WORK_PER_TASK);

    /* Compute reference sequentially */
    double ref_total = 0.0;
    for (int i = 0; i < NTASKS; i++) {
        double sum = 0.0;
        for (int j = 0; j < WORK_PER_TASK; j++) {
            sum += sin((double)(i * WORK_PER_TASK + j) * 0.001);
        }
        ref_total += sum;
    }

    /* Compute with OpenMP tasks */
    #pragma omp parallel
    {
        #pragma omp single
        {
            for (int i = 0; i < NTASKS; i++) {
                #pragma omp task firstprivate(i)
                {
                    double sum = 0.0;
                    for (int j = 0; j < WORK_PER_TASK; j++) {
                        sum += sin((double)(i * WORK_PER_TASK + j) * 0.001);
                    }
                    results[i] = sum;
                }
            }
        }
        /* implicit barrier + taskwait at end of parallel */
    }

    double task_total = 0.0;
    for (int i = 0; i < NTASKS; i++) task_total += results[i];

    printf("  Reference:  %.10f\n", ref_total);
    printf("  Tasks:      %.10f\n", task_total);
    printf("  Match: %s (diff = %.1e)\n",
           fabs(task_total - ref_total) < 1e-6 ? "YES" : "NO",
           fabs(task_total - ref_total));

    /* --- Test 2: Performance --- */
    printf("\n--- Test 2: Performance ---\n");

    /* Sequential baseline */
    double t0 = omp_get_wtime();
    double seq_total = 0.0;
    for (int i = 0; i < NTASKS; i++) {
        double sum = 0.0;
        for (int j = 0; j < WORK_PER_TASK; j++) {
            sum += sin((double)(i * WORK_PER_TASK + j) * 0.001);
        }
        seq_total += sum;
    }
    double t1 = omp_get_wtime();
    double seq_ms = (t1 - t0) * 1000;

    /* Parallel tasks */
    double t2 = omp_get_wtime();
    #pragma omp parallel
    {
        #pragma omp single
        {
            for (int i = 0; i < NTASKS; i++) {
                #pragma omp task firstprivate(i)
                {
                    double sum = 0.0;
                    for (int j = 0; j < WORK_PER_TASK; j++) {
                        sum += sin((double)(i * WORK_PER_TASK + j) * 0.001);
                    }
                    results[i] = sum;
                }
            }
        }
    }
    double t3 = omp_get_wtime();
    double par_ms = (t3 - t2) * 1000;

    printf("  Sequential:  %.1f ms\n", seq_ms);
    printf("  Tasks (%dT):  %.1f ms\n", nthreads, par_ms);
    printf("  Speedup:     %.2fx\n", seq_ms / par_ms);

    printf("\n=== Done ===\n");
    return 0;
}
