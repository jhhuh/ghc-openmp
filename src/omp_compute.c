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
