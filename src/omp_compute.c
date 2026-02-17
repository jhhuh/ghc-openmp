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
