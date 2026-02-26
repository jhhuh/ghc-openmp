/*
 * omp_shared.c — OpenMP kernels for shared-memory demos.
 *
 * Element-wise array transform: out[i] = f(in[i])
 * where f(x) = sin(x)*cos(x) + sqrt(fabs(x))
 *
 * Three entry points:
 *   transform_all           — transform all N elements (Demo 1)
 *   transform_range         — transform [offset..offset+len) (Demo 2/3)
 *   transform_range_barrier — same + explicit GOMP_barrier (Demo 2)
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
