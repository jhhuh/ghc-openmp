/*
 * bench_overhead.c — Microbenchmarks for OpenMP runtime overhead
 *
 * Measures:
 * 1. Parallel region fork/join overhead (empty parallel region)
 * 2. Barrier latency
 * 3. Parallel for with reduction (compute-bound)
 * 4. Critical section contention
 * 5. Task creation/execution overhead
 *
 * Compile: gcc -fopenmp -O2 -o bench bench_overhead.c -lm
 * Run:     OMP_NUM_THREADS=4 ./bench
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>

#define WARMUP_ITERS 100
#define BENCH_ITERS  10000

/* ---- Benchmark 1: Fork/join overhead ---- */
static double bench_fork_join(int iters) {
    double t0 = omp_get_wtime();
    for (int i = 0; i < iters; i++) {
        #pragma omp parallel
        {
            /* empty */
            __asm__ volatile("" ::: "memory");
        }
    }
    double t1 = omp_get_wtime();
    return (t1 - t0) / iters;
}

/* ---- Benchmark 2: Barrier latency ---- */
static double bench_barrier(int iters) {
    double t0, t1;
    #pragma omp parallel
    {
        #pragma omp barrier
        #pragma omp master
        { t0 = omp_get_wtime(); }
        #pragma omp barrier

        for (int i = 0; i < iters; i++) {
            #pragma omp barrier
        }

        #pragma omp barrier
        #pragma omp master
        { t1 = omp_get_wtime(); }
    }
    return (t1 - t0) / iters;
}

/* ---- Benchmark 3: Parallel for + reduction ---- */
#define PF_ITERS 10  /* run parallel for multiple times for stable measurement */

static double bench_parallel_for(int n) {
    double best = 1e9;
    for (int run = 0; run < PF_ITERS; run++) {
        double sum = 0.0;
        double t0 = omp_get_wtime();
        #pragma omp parallel for reduction(+:sum) schedule(static)
        for (int i = 0; i < n; i++) {
            sum += sin((double)i * 0.001);
        }
        double t1 = omp_get_wtime();
        double elapsed = t1 - t0;
        if (elapsed < best) best = elapsed;
        /* prevent dead-code elimination */
        if (sum == -999.999) printf("%f\n", sum);
    }
    return best;
}

/* ---- Benchmark 4: Critical section ---- */
static double bench_critical(int iters_per_thread) {
    volatile int counter = 0;
    double t0 = omp_get_wtime();
    #pragma omp parallel
    {
        for (int i = 0; i < iters_per_thread; i++) {
            #pragma omp critical
            {
                counter++;
            }
        }
    }
    double t1 = omp_get_wtime();
    (void)counter;
    return t1 - t0;
}

/* ---- Benchmark 5: Task overhead ---- */
static volatile int task_sink = 0;

static double bench_tasks(int ntasks) {
    double t0 = omp_get_wtime();
    #pragma omp parallel
    {
        #pragma omp single
        {
            for (int i = 0; i < ntasks; i++) {
                #pragma omp task
                {
                    __asm__ volatile("" ::: "memory");
                    task_sink++;
                }
            }
            #pragma omp taskwait
        }
    }
    double t1 = omp_get_wtime();
    return t1 - t0;
}

int main(void) {
    int nthreads;
    #pragma omp parallel
    {
        #pragma omp master
        nthreads = omp_get_num_threads();
    }

    printf("OpenMP Runtime Benchmark\n");
    printf("Threads: %d\n\n", nthreads);

    /* Warmup */
    bench_fork_join(WARMUP_ITERS);
    bench_barrier(WARMUP_ITERS);

    /* 1. Fork/join */
    double fj = bench_fork_join(BENCH_ITERS);
    printf("1. Fork/join overhead:      %8.3f us/iter\n", fj * 1e6);

    /* 2. Barrier */
    double bar = bench_barrier(BENCH_ITERS);
    printf("2. Barrier latency:         %8.3f us/iter\n", bar * 1e6);

    /* 3. Parallel for (1M iterations with sin()) */
    double pf = bench_parallel_for(1000000);
    printf("3. Parallel for (1M sin):   %8.3f ms\n", pf * 1e3);

    /* 4. Critical section (1000 per thread) */
    double cr = bench_critical(1000);
    printf("4. Critical (1000/thread):  %8.3f ms\n", cr * 1e3);

    /* 5. Tasks (10000 tasks) */
    double tk = bench_tasks(10000);
    printf("5. Tasks (10000):           %8.3f ms\n", tk * 1e3);

    return 0;
}
