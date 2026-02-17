/*
 * bench_dgemm.c — Head-to-head DGEMM benchmark
 *
 * Compiles against either native libgomp or our RTS-backed runtime.
 * Measures parallel DGEMM performance at multiple matrix sizes,
 * best-of-3 to reduce variance.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <omp.h>

#define WARMUP_ITERS 1
#define TIMED_ITERS  3

/* Dense matrix multiply: C = A * B (row-major, NxN) */
static void dgemm(const double *A, const double *B, double *C, int n) {
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

/* Fill matrix with deterministic values */
static void fill_matrix(double *M, int n, double scale, double offset) {
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
            M[i * n + j] = (i + 1) * scale + j * offset;
}

/* Compute checksum (Frobenius norm) for verification */
static double frobenius(const double *M, int n) {
    double sum = 0.0;
    for (int i = 0; i < n * n; i++)
        sum += M[i] * M[i];
    return sqrt(sum);
}

static double bench_size(int n, double *A, double *B, double *C) {
    /* Warmup */
    for (int i = 0; i < WARMUP_ITERS; i++)
        dgemm(A, B, C, n);

    /* Timed: best of TIMED_ITERS */
    double best = 1e9;
    for (int i = 0; i < TIMED_ITERS; i++) {
        double t0 = omp_get_wtime();
        dgemm(A, B, C, n);
        double t1 = omp_get_wtime();
        double elapsed = t1 - t0;
        if (elapsed < best) best = elapsed;
    }
    return best;
}

int main(void) {
    int nthreads;
    #pragma omp parallel
    {
        #pragma omp master
        nthreads = omp_get_num_threads();
    }

    printf("DGEMM Benchmark (%d threads)\n", nthreads);
    printf("%-6s  %10s  %10s  %8s\n", "N", "Time (ms)", "GFLOPS", "Checksum");

    int sizes[] = {128, 256, 512, 1024};
    int nsizes = sizeof(sizes) / sizeof(sizes[0]);

    /* Allocate for largest size */
    int maxn = sizes[nsizes - 1];
    double *A = malloc(maxn * maxn * sizeof(double));
    double *B = malloc(maxn * maxn * sizeof(double));
    double *C = malloc(maxn * maxn * sizeof(double));

    if (!A || !B || !C) {
        fprintf(stderr, "malloc failed\n");
        return 1;
    }

    for (int s = 0; s < nsizes; s++) {
        int n = sizes[s];
        fill_matrix(A, n, 0.01, 0.001);
        fill_matrix(B, n, 0.01, -0.001);
        memset(C, 0, n * n * sizeof(double));

        double elapsed = bench_size(n, A, B, C);
        double flops = 2.0 * n * n * n;  /* 2N^3 for matmul */
        double gflops = flops / elapsed / 1e9;
        double chk = frobenius(C, n);

        printf("%-6d  %10.2f  %10.2f  %8.2f\n",
               n, elapsed * 1000.0, gflops, chk);
    }

    free(A);
    free(B);
    free(C);

    return 0;
}
