## 1. Abstract

We implement a drop-in OpenMP runtime library that uses GHC's Runtime System
as its threading infrastructure. Standard C code compiled with
`gcc -fopenmp` runs on GHC Capabilities instead of libgomp's
pthreads. The runtime implements the GCC `GOMP_*` ABI and the
`omp_*` user API, supporting parallel regions, worksharing loops,
barriers, critical sections, tasks, and sections.

After lock-free optimization, the runtime achieves **performance parity
with native libgomp** on both microbenchmarks and real numerical workloads
(dense matrix multiplication). Haskell programs call OpenMP-parallelized C code
via FFI, with both runtimes sharing the same thread pool. OpenMP workers
call back into Haskell via `FunPtr` with automatic Capability
acquisition. GHC's stop-the-world garbage collector does not pause OpenMP
workers because they do not hold Capabilities. The culmination is
type-safe shared memory: using GHC's linear types, Haskell and OpenMP C
code operate on disjoint regions of the same array with compile-time
proof of safety and zero synchronization overhead.

---

