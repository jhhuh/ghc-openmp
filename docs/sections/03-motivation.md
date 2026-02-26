## Motivation {#sec:motivation}

GHC's RTS has a mature, production-quality thread pool with per-Capability
run queues, work-stealing spark pools, NUMA awareness, and sophisticated
scheduling. OpenMP runtimes (libgomp, libomp) maintain their own, separate
thread pools. When a Haskell program calls OpenMP-annotated C code via FFI,
two independent thread pools compete for the same CPU cores.

If the OpenMP runtime used GHC's thread pool directly, we would get:

- **Unified resource management** — one thread pool, not two
- **Seamless interop** — Haskell green threads and OpenMP parallel regions coexist naturally
- **GHC as a platform** — C programs benefit from GHC's scheduler, and Haskell programs get access to OpenMP's parallel-for without reinventing it
- **Type-safe shared memory** — linear types can prove disjoint access at compile time, eliminating synchronization barriers when Haskell and C share data

This project investigates whether this is feasible and what the performance
cost is.

---

