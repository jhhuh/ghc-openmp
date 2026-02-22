/*
 * omp_compute.c — OpenMP-parallelized compute functions callable from Haskell
 *
 * These are ordinary C functions using OpenMP pragmas, compiled with -fopenmp.
 * When linked against our RTS-backed runtime, they use GHC Capabilities.
 */

#include <math.h>
#include <omp.h>

/* Parallel dot product: sum(a[i] * b[i]) */
double parallel_dot(const double *a, const double *b, int n) {
    double sum = 0.0;
    #pragma omp parallel for reduction(+:sum) schedule(static)
    for (int i = 0; i < n; i++) {
        sum += a[i] * b[i];
    }
    return sum;
}

/* Parallel SAXPY: y[i] = alpha * x[i] + y[i] */
void parallel_saxpy(double alpha, const double *x, double *y, int n) {
    #pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        y[i] = alpha * x[i] + y[i];
    }
}

/* Parallel sum of sin() — compute-bound benchmark */
double parallel_sinsum(int n) {
    double sum = 0.0;
    #pragma omp parallel for reduction(+:sum) schedule(static)
    for (int i = 0; i < n; i++) {
        sum += sin((double)i * 0.001);
    }
    return sum;
}

/* Run many short OpenMP regions, record per-iteration timing.
 * Returns max latency in microseconds. */
double repeated_parallel_sinsum(int n_per_iter, int iters, double *times_us) {
    double total = 0.0;
    for (int r = 0; r < iters; r++) {
        double t0 = omp_get_wtime();
        double sum = 0.0;
        #pragma omp parallel for reduction(+:sum) schedule(static)
        for (int i = 0; i < n_per_iter; i++) {
            sum += sin((double)i * 0.001);
        }
        double t1 = omp_get_wtime();
        total += sum;
        if (times_us) times_us[r] = (t1 - t0) * 1e6;
    }
    return total;
}

/* Sequential sum of sin() — no OpenMP, for crossover analysis */
double sequential_sinsum(int n) {
    double sum = 0.0;
    for (int i = 0; i < n; i++) {
        sum += sin((double)i * 0.001);
    }
    return sum;
}

/* Dense matrix multiply: C = A * B (row-major, NxN)
 * Parallelizes over rows of C. */
void parallel_dgemm(const double *A, const double *B, double *C, int n) {
    #pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            double sum = 0.0;
            for (int k = 0; k < n; k++) {
                sum += A[i * n + k] * B[k * n + j];
            }
            C[i * n + j] = sum;
        }
    }
}

/* ---- Bidirectional interop: OpenMP workers call Haskell callbacks ---- */

/* Type for a Haskell callback: takes index, returns double */
typedef double (*hs_callback_t)(int);

/* Parallel map with Haskell callback: out[i] = callback(i)
 * Each OpenMP thread calls back into Haskell for every element. */
void parallel_map_callback(hs_callback_t callback, double *out, int n) {
    #pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        out[i] = callback(i);
    }
}

/* Parallel reduce with Haskell callback: sum(callback(i) for i in 0..n-1) */
double parallel_reduce_callback(hs_callback_t callback, int n) {
    double sum = 0.0;
    #pragma omp parallel for reduction(+:sum) schedule(static)
    for (int i = 0; i < n; i++) {
        sum += callback(i);
    }
    return sum;
}

/* ---- Task benchmarks: deferred execution ---- */

/* Sequential: compute sinsum tasks one-by-one */
double run_sequential_benchmark(int ntasks, int work_per_task) {
    double total = 0.0;
    for (int i = 0; i < ntasks; i++) {
        double sum = 0.0;
        for (int j = 0; j < work_per_task; j++) {
            sum += sin((double)(i * work_per_task + j) * 0.001);
        }
        total += sum;
    }
    return total;
}

/* Parallel: create ntasks OpenMP tasks inside a single block.
 * With deferred execution, idle threads steal tasks from the queue. */
static double g_task_results[100000]; /* pre-allocated result array */

double run_task_benchmark(int ntasks, int work_per_task) {
    #pragma omp parallel
    {
        #pragma omp single
        {
            for (int i = 0; i < ntasks; i++) {
                #pragma omp task firstprivate(i)
                {
                    double sum = 0.0;
                    for (int j = 0; j < work_per_task; j++) {
                        sum += sin((double)(i * work_per_task + j) * 0.001);
                    }
                    g_task_results[i] = sum;
                }
            }
        }
        /* implicit barrier — threads steal tasks here */
    }

    double total = 0.0;
    for (int i = 0; i < ntasks; i++) total += g_task_results[i];
    return total;
}

/* Query runtime info */
int get_omp_num_threads(void) {
    int n;
    #pragma omp parallel
    {
        #pragma omp master
        n = omp_get_num_threads();
    }
    return n;
}
