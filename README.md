# ghc-openmp: GHC's Runtime System as an OpenMP Runtime

> **Note:** This is an experimental project. The claims and results have not been
> thoroughly reviewed and should be considered tentative. The code, experiment
> design, and execution were conducted with significant assistance from
> [Claude Code](https://docs.anthropic.com/en/docs/claude-code).

An experimental OpenMP runtime that uses GHC's Runtime System (RTS) as its
thread pool and scheduler infrastructure. Standard OpenMP C code compiled with
`gcc -fopenmp` runs on GHC Capabilities instead of libgomp's pthreads, enabling
seamless interoperation between Haskell and OpenMP-parallelized C code.

## Key Results

- **Performance parity** with native libgomp across all benchmarks (fork/join,
  barrier, parallel for, DGEMM)
- **Haskell FFI interop**: Haskell programs call OpenMP C code via `foreign
  import ccall safe`, with both running on the same thread pool
- **Concurrent execution**: Haskell green threads and OpenMP parallel regions
  run simultaneously without starving each other
- **GC isolation**: GHC's stop-the-world GC does not pause OpenMP workers
  (workers don't hold Capabilities)
- **Bidirectional FFI**: OpenMP workers call back into Haskell via FunPtr,
  with automatic Capability acquisition

## Architecture

```
Haskell program (or C host)
        |
        | foreign import ccall safe / direct call
        v
C code with #pragma omp parallel for
        |
        | calls GOMP_parallel(fn, data, N, flags)
        v
ghc_omp_runtime_rts.c  (our OpenMP runtime)
        |
        | dispatches to worker pool
        v
GHC RTS Capabilities (OS threads)
  Cap 0    Cap 1    Cap 2    Cap 3
  (master) (worker) (worker) (worker)
```

OpenMP workers are permanent OS threads pinned to GHC Capabilities via
`rts_setInCallCapability()`. After initialization, they do **not** hold
Capabilities — they are plain OS threads spinning on atomic variables,
invisible to GHC's garbage collector.

## Documentation

Full write-up with charts and benchmarks: **https://jhhuh.github.io/ghc-openmp/**

Built with Hakyll and the Haskell Chart library (static SVGs at build time, no client-side JS).

```bash
nix build .#docs       # Build static site to ./result/
nix run .#docs         # Serve locally at http://localhost:8080
```

## Quick Start

### Prerequisites

- [Nix](https://nixos.org/) with flakes enabled (provides GHC, GCC, all dependencies)

### Build & Run

```bash
# Build all binaries
nix build

# Run tests and benchmarks
nix run .#test-all
nix run .#bench
nix run .#test-tasks
nix run .#bench-dgemm

# Interactive development
nix develop
make all
make test-all
```

## Project Structure

```
src/
  ghc_omp_runtime_rts.c   # The OpenMP runtime (RTS-backed, ~1300 lines)
  ghc_omp_runtime.c        # Phase 1 stub runtime (pthread-based, reference)
  omp_compute.c             # OpenMP compute kernels (dot, saxpy, sinsum, dgemm)
  HsMain.hs                 # Phase 4: Haskell FFI interop demo
  HsConcurrent.hs           # Phase 5: concurrent Haskell + OpenMP
  HsGCStress.hs             # Phase 6: GC interaction test
  HsMatMul.hs               # Phase 7: dense matrix multiply
  HsCallback.hs             # Phase 9: bidirectional interop (OpenMP -> Haskell)
  omp_prims.cmm              # Phase 10: Cmm primitives (zero-overhead RTS access)
  HsCmmDemo.hs               # Phase 10: calling convention benchmark
  HsInlineCmm.hs             # Phase 11: inline-cmm quasiquoter demo
  omp_batch.cmm               # Phase 12: batched safe calls (manual suspend/resume)
  HsCmmBatch.hs               # Phase 12: batch overhead benchmark
  HsCrossover.hs              # Phase 13: parallelism crossover analysis
  HsParCompare.hs             # Phase 14: GHC forkIO vs OpenMP comparison
  HsTaskDemo.hs               # Phase 15: deferred task execution benchmark
  test_omp_tasks.c            # Phase 15: C-level task correctness/perf test
  HsZeroCopy.hs               # Phase 16: zero-copy FFI with pinned ByteArray
  Data/Array/Linear.hs        # Phase 17: linear typed arrays (inspired by konn/linear-extra)
  HsLinearDemo.hs             # Phase 17: type-safe parallel sub-array demo
  bench_overhead.c           # Microbenchmark suite
  bench_dgemm.c              # DGEMM benchmark (native vs RTS)
  test_omp_basic.c           # Basic OpenMP construct tests
  test_rts_embed.c           # GHC RTS embedding test
  test_guided.c              # Phase 18: guided scheduling correctness test
  test_nested.c              # Phase 18: nested parallelism level tracking test
  HsStub.hs                  # Minimal Haskell module for RTS initialization

artifacts/                  # Research notes, plans, and benchmark results
```

## Implemented OpenMP Features

| Feature | Status |
|---------|--------|
| `#pragma omp parallel` | Full |
| `#pragma omp parallel for` (static, dynamic, guided) | Full |
| `#pragma omp barrier` | Full (sense-reversing, lock-free) |
| `#pragma omp critical` (named and unnamed) | Full |
| `#pragma omp single` | Full |
| `#pragma omp atomic` | Fallback mutex |
| `#pragma omp task` / `taskwait` | Full (deferred + work-stealing) |
| `#pragma omp sections` | Full |
| `#pragma omp ordered` | Mutex-based |
| `omp_*` user API (threads, locks, timing) | Full |
| Nested parallelism | Serialized (inner regions run single-threaded) |
| Target offloading | Not applicable |

## Benchmark Results (4 threads, i7-10750H)

### Microbenchmarks

| Metric | Native libgomp | RTS-backed | Ratio |
|--------|---------------|------------|-------|
| Fork/join | 0.97 us | 0.81 us | **0.83x** (RTS faster) |
| Barrier | 0.51 us | 0.25 us | **0.50x** (RTS faster) |
| Parallel for (1M sin) | 3.85 ms | 3.91 ms | 1.01x (parity) |
| Critical section | 0.92 ms | 0.38 ms | **0.42x** (RTS faster) |

### DGEMM (dense matrix multiply)

| N | Native (ms) | RTS (ms) | Ratio |
|---|------------|---------|-------|
| 512 | 77.5 | 77.0 | 0.99x |
| 1024 | 748.8 | 663.4 | 0.89x |

Performance is indistinguishable within measurement noise.

## How It Works

1. **RTS Boot**: On first `GOMP_parallel` call, `hs_init_ghc()` initializes
   the GHC RTS (or increments its ref count if already running from Haskell).

2. **Worker Pool**: N-1 OS threads are created and pinned to Capabilities
   1..N-1. Each does `rts_lock(); rts_unlock();` once to register with the
   RTS, then enters a spin-wait loop.

3. **Parallel Region**: Master stores work item (function pointer + data),
   increments an atomic generation counter. Workers detect the generation
   change, participate in a sense-reversing start barrier, execute the
   function, then hit the end barrier.

4. **Synchronization**: Lock-free sense-reversing centralized barriers with
   spin-wait (~4000 iterations) and condvar fallback for power efficiency.

5. **Haskell Interop**: `foreign import ccall safe` releases the calling
   Capability, so other Haskell green threads run while OpenMP executes.
   Workers don't hold Capabilities, making them invisible to GC.

## Cmm Primitives and inline-cmm

Phase 10 uses [Cmm](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/cmm-type)
(GHC's low-level intermediate representation) to write primitives callable from
Haskell via `foreign import prim`. This is the fastest calling convention GHC
offers — arguments pass directly in STG registers with no FFI boundary at all.

For example, reading the current Capability number (equivalent to
`omp_get_thread_num()`) compiles to a single memory load:

```cmm
#include "Cmm.h"

omp_prim_cap_no(W_ dummy) {
    return (Capability_no(MyCapability()));
}
```

```haskell
foreign import prim "omp_prim_cap_no" primCapNo# :: Int# -> Int#
```

The [`inline-cmm`](https://github.com/jhhuh/inline-cmm) library automates this
pattern, letting you embed Cmm code directly in Haskell modules via a
`[cmm| ... |]` quasiquoter — similar to how `inline-c` handles C. It
automatically generates the `foreign import prim` declaration and compiles the
Cmm to an object file via Template Haskell.

### Calling Convention Overhead

| Convention | ns/call | Notes |
|---|---|---|
| `foreign import prim` (Cmm) | ~0 | GHC can optimize away (LICM, CSE) |
| `foreign import ccall unsafe` | ~2 | STG register save/restore |
| `foreign import ccall safe` | ~68 | + Capability release/reacquire |

### Batched Safe Calls

Phase 12 shows that the ~68ns safe FFI overhead can be amortized by batching
multiple C calls within a single `suspendThread`/`resumeThread` cycle, written
manually in Cmm:

| Batch size | Per-call cost | Speedup vs safe |
|---|---|---|
| 1 | 69 ns | 1.0x |
| 10 | 8.7 ns | 8.2x |
| 100 | 2.7 ns | 27x |

At batch=100, per-call overhead approaches unsafe FFI cost (~2 ns).

## License

Experimental research project.
