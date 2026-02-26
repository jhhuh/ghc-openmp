/*
 * omp_shared.c — OpenMP kernels for shared-memory demos.
 *
 * Element-wise array transform: out[i] = f(in[i])
 * where f(x) = sin(x)*cos(x) + sqrt(fabs(x))
 *
 * Entry points for Demos 1-3:
 *   transform_all                  — transform all N elements (Demo 1)
 *   transform_range                — transform [offset..offset+len) (Demo 2/3)
 *   transform_range_barrier        — same + explicit GOMP_barrier (Demo 2)
 *   transform_partitioned_barrier  — P partitions, real barriers (Demo 2/3)
 *   transform_partitioned_nobarrier— P partitions, nowait (Demo 2/3)
 *
 * Entry points for Demo 4 (safety):
 *   transform_overlap_nowait       — overlapping partitions + nowait (BUG)
 *   transform_overlap_barrier      — overlapping partitions + barrier (deterministic)
 *   stencil_barrier                — two-pass with barrier (correct)
 *   stencil_nowait                 — two-pass without barrier (BUG)
 */

#include <math.h>
#include <omp.h>

/* Non-trivial transform to prevent compiler from optimizing away */
static double transform_elem(double x) {
    return sin(x) * cos(x) + sqrt(fabs(x));
}

/* Transform all N elements in parallel */
void transform_all(const double *in, double *out, int n) {
    #pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++)
        out[i] = transform_elem(in[i]);
}

/* Transform elements [offset..offset+len) in parallel */
void transform_range(const double *in, double *out, int offset, int len) {
    #pragma omp parallel for schedule(static)
    for (int i = 0; i < len; i++)
        out[offset + i] = transform_elem(in[offset + i]);
}

/* Same as transform_range but calls GOMP_barrier() before returning.
 * This is the "defensive synchronization" that Demo 2 measures. */
extern void GOMP_barrier(void);

void transform_range_barrier(const double *in, double *out, int offset, int len) {
    #pragma omp parallel for schedule(static)
    for (int i = 0; i < len; i++)
        out[offset + i] = transform_elem(in[offset + i]);
    GOMP_barrier();
}

/* Process N elements in P partitions inside one parallel region,
 * repeated for `iters` iterations.  One parallel region is created;
 * the iteration and partition loops run inside it.
 *
 * Each partition phase uses #pragma omp for schedule(static).
 * The implicit barrier at the end of each #pragma omp for ensures
 * all threads complete one partition before starting the next.
 * Total barriers = iters * num_parts. */
void transform_partitioned_barrier(const double *in, double *out,
                                   int n, int num_parts, int iters) {
    int chunk = n / num_parts;
    #pragma omp parallel
    {
        for (int it = 0; it < iters; it++) {
            for (int p = 0; p < num_parts; p++) {
                int off = p * chunk;
                int len = (p == num_parts - 1) ? n - off : chunk;
                #pragma omp for schedule(static)
                for (int i = 0; i < len; i++)
                    out[off + i] = transform_elem(in[off + i]);
            }
        }
    }
}

/* Same as above but with nowait — no barriers between partitions.
 * Only safe when partitions are provably disjoint (they always are here,
 * but the compiler doesn't know that without linear types). */
void transform_partitioned_nobarrier(const double *in, double *out,
                                     int n, int num_parts, int iters) {
    int chunk = n / num_parts;
    #pragma omp parallel
    {
        for (int it = 0; it < iters; it++) {
            for (int p = 0; p < num_parts; p++) {
                int off = p * chunk;
                int len = (p == num_parts - 1) ? n - off : chunk;
                #pragma omp for schedule(static) nowait
                for (int i = 0; i < len; i++)
                    out[off + i] = transform_elem(in[off + i]);
            }
        }
    }
}

/* =========================================================================
 * Demo 4: Safety — overlap bugs and stencils
 * ========================================================================= */

/* Overlapping partitions: each partition accumulates [off..off+chunk] instead
 * of [off..off+chunk).  The "+1" overlap means boundary elements are
 * accumulated by two partitions — a data race with nowait.
 * Uses += to make the double-write detectable: boundary elements get 2*f(x)
 * instead of f(x). With nowait, the race on += may also lose updates. */
void transform_overlap_nowait(const double *in, double *out,
                              int n, int num_parts) {
    int chunk = n / num_parts;
    /* Initialize output to zero so += accumulates correctly */
    for (int i = 0; i < n; i++) out[i] = 0.0;
    #pragma omp parallel
    {
        for (int p = 0; p < num_parts; p++) {
            int off = p * chunk;
            /* BUG: +1 overlap — last element bleeds into next partition */
            int len = (p == num_parts - 1) ? n - off : chunk + 1;
            #pragma omp for schedule(static) nowait
            for (int i = 0; i < len; i++)
                out[off + i] += transform_elem(in[off + i]);
        }
    }
}

/* Same overlap with barriers — no data race, but boundary elements are
 * still accumulated twice (deterministic but wrong: 2*f(x) at boundaries). */
void transform_overlap_barrier(const double *in, double *out,
                               int n, int num_parts) {
    int chunk = n / num_parts;
    for (int i = 0; i < n; i++) out[i] = 0.0;
    #pragma omp parallel
    {
        for (int p = 0; p < num_parts; p++) {
            int off = p * chunk;
            int len = (p == num_parts - 1) ? n - off : chunk + 1;
            #pragma omp for schedule(static)
            for (int i = 0; i < len; i++)
                out[off + i] += transform_elem(in[off + i]);
        }
    }
}

/* Two-pass stencil WITH barrier between passes.
 * Pass 1: out[i] = f(in[i])                    — independent per element
 * Pass 2: out[i] = (out[i-1] + out[i] + out[i+1]) / 3.0  — reads neighbors
 *
 * Two separate parallel-for regions: the implicit barrier at the end of
 * the first ensures all of pass 1 is complete before pass 2 starts. */
void stencil_barrier(const double *in, double *out, int n, int num_parts) {
    (void)num_parts;
    /* Pass 1: independent transform — implicit barrier at end */
    #pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++)
        out[i] = transform_elem(in[i]);
    /* Pass 2: 3-point stencil averaging — all pass 1 values visible */
    #pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        double left  = (i > 0)     ? out[i - 1] : out[i];
        double right = (i < n - 1) ? out[i + 1] : out[i];
        out[i] = (left + out[i] + right) / 3.0;
    }
}

/* Same two-pass stencil but WITHOUT barrier between passes.
 * Single parallel region with nowait: a fast thread can finish pass 1
 * and start pass 2 while slow threads still execute pass 1.
 * schedule(dynamic) increases race probability by varying which thread
 * handles which elements across the two passes. */
void stencil_nowait(const double *in, double *out, int n, int num_parts) {
    (void)num_parts;
    for (int i = 0; i < n; i++) out[i] = 0.0;
    #pragma omp parallel
    {
        /* Pass 1: nowait — threads proceed to pass 2 independently */
        #pragma omp for schedule(dynamic, 64) nowait
        for (int i = 0; i < n; i++)
            out[i] = transform_elem(in[i]);
        /* BUG: no barrier — pass 2 may read stale/zero values */
        #pragma omp for schedule(dynamic, 64) nowait
        for (int i = 0; i < n; i++) {
            double left  = (i > 0)     ? out[i - 1] : out[i];
            double right = (i < n - 1) ? out[i + 1] : out[i];
            out[i] = (left + out[i] + right) / 3.0;
        }
    }
}
