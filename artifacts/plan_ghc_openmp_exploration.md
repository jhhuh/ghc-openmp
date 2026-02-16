# Plan: GHC Runtime as OpenMP Runtime — Exploration

## Goal
Investigate the feasibility of using GHC's runtime system (RTS) to implement
an OpenMP-compatible runtime library, replacing libgomp (GCC) or libomp (LLVM).

## Motivation
- GHC RTS has a mature **work-stealing scheduler** with per-capability run queues
- **Sparks** are extremely lightweight (pointer-sized entries in a bounded deque)
- GHC already solves load balancing, thread migration, and safe foreign calls
- If successful, this enables Haskell programs to seamlessly interop with
  OpenMP-annotated C/Fortran code, or exposes GHC's scheduler to C programs

## Phase 1: Research & API Surface Mapping
1. **Study GHC RTS internals** — focus on:
   - `rts/Schedule.c` — the scheduler loop
   - `rts/Sparks.c` — spark pool / work-stealing deque
   - `rts/Capability.h` — per-OS-thread execution context
   - `rts/Task.h` — OS thread management
   - `rts/sm/` — how GC interacts with parallelism
2. **Map the OpenMP Runtime API** — catalog what needs implementing:
   - **libgomp** (GCC): `GOMP_parallel`, `GOMP_barrier`, `GOMP_loop_*`, `GOMP_task`, etc.
   - **libomp** (LLVM): `__kmpc_fork_call`, `__kmpc_barrier`, `__kmpc_for_static_init_*`, etc.
   - **omp.h user API**: `omp_get_num_threads`, `omp_get_thread_num`, `omp_set_num_threads`, locks, etc.
3. **Identify semantic gaps** between GHC's model and OpenMP's model

## Phase 2: Minimal Prototype
Target: implement enough to run `#pragma omp parallel` and `#pragma omp parallel for`
with GCC (libgomp ABI) or Clang (libomp ABI).

Approach options (to decide):
- **Option A**: C shim library that calls GHC RTS C API (`rts/Schedule.h`, `rts/Sparks.h`)
- **Option B**: Haskell `foreign export ccall` that exposes OpenMP entry points
- **Option C**: Modify GHC RTS source directly to add OpenMP entry points

## Phase 3: Benchmarking
- Compare against native libgomp/libomp on standard OpenMP benchmarks
  (EPCC microbenchmarks, NAS Parallel Benchmarks, SPEC OMP)
- Measure overhead of GHC's GC pauses on OpenMP workloads

## Key Technical Challenges
| Challenge | Notes |
|---|---|
| Thread IDs | OpenMP assigns deterministic 0..N-1 IDs; GHC threads don't have this |
| Thread teams | OpenMP fork-join model vs GHC's persistent thread pool |
| Worksharing | `omp for` needs deterministic iteration partitioning to numbered threads |
| Thread-local storage | `threadprivate` maps to OS TLS; GHC green threads don't have OS TLS |
| Reductions | Need to map OpenMP reduction clauses to per-capability accumulators |
| Nested parallelism | OpenMP supports nested parallel regions; GHC caps at +RTS -N |
| Ordering | `omp ordered` construct needs thread-ID-based sequencing |

## Decision: Which ABI to target?
**Recommendation: libgomp (GCC)** — smaller, simpler API surface, well-documented in
GCC source. libomp (__kmpc) is more complex with microtask function pointers.

## Current Phase
**Phase 1** — Research & API surface mapping.

## Next Actions
- [ ] Fetch GHC RTS source and study scheduler entry points
- [ ] Catalog full libgomp ABI (from gcc/libgomp sources)
- [ ] Write a minimal "hello world" that calls GOMP_parallel with a custom libgomp.so
- [ ] Decide on approach (Option A/B/C)
