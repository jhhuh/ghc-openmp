# ghc-openmp: GHC's Runtime System as an OpenMP Runtime

An OpenMP runtime that uses GHC's Runtime System (RTS) as its
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
  Cap 0    Cap 1    Cap 2    ...    Cap N-1
  (master) (worker) (worker)        (worker)
```

OpenMP workers are permanent OS threads pinned to GHC Capabilities via
`rts_setInCallCapability()`. After initialization, they do **not** hold
Capabilities — they are plain OS threads spinning on atomic variables,
invisible to GHC's garbage collector.

## Documentation

Full write-up with charts and benchmarks: **https://jhhuh.github.io/ghc-openmp/**

Built with MkDocs Material and the Haskell Chart library (static SVGs at build time, no client-side JS).

```bash
nix build .#docs       # Build static site to ./result/
nix run .#docs         # Serve locally at http://localhost:8080
```

## Building

### With Nix (recommended)

```bash
nix build              # Build all binaries (libghcomp.so, tests, benchmarks, demos)
nix run .#test-all     # Run all tests
nix run .#bench        # Run microbenchmarks
nix run .#bench-dgemm  # Run DGEMM benchmark
nix develop            # Enter dev shell with GHC, GCC, and tools
```

### Without Nix

Prerequisites: GHC (with threaded RTS), GCC (with OpenMP support), make.

```bash
make all               # Build libghcomp.so and basic tests
make build-all         # Build everything (tests, benchmarks, demos)
make test-all          # Run all tests
make bench             # Run microbenchmarks
```

The Makefile auto-discovers GHC RTS include/library paths via `ghc --print-libdir`.

### As a Haskell library (cabal)

Add to your `.cabal` file:

```cabal
build-depends: ghc-openmp
ghc-options:   -threaded
```

The C runtime source is compiled directly into your package using your own
GHC — no shared library linkage, no ABI conflicts.

```haskell
import GHC.OpenMP

-- Call your OpenMP C code via safe FFI
foreign import ccall safe "my_parallel_function"
    c_myFunction :: CInt -> IO CDouble
```

## Drop-in libgomp Replacement (C projects)

`libghcomp.so` is a drop-in replacement for `libgomp.so`. Any C program
compiled with `gcc -fopenmp` can use it without source changes.

### Build the shared library

```bash
# With Nix:
nix build
ls result/lib/libghcomp.so

# Without Nix:
make build/libghcomp.so
```

### Link against it

```bash
# Compile your OpenMP program, linking against libghcomp instead of libgomp
gcc -fopenmp my_program.c -Lresult/lib -lghcomp -Wl,-rpath,result/lib -o my_program
./my_program
```

### LD_PRELOAD (no recompilation)

```bash
# Use with an existing binary — replaces libgomp at load time
LD_PRELOAD=result/lib/libghcomp.so ./my_existing_omp_program
```

### pkg-config

A `ghcomp.pc.in` template is shipped in `data/`. After installation:

```bash
gcc -fopenmp my_code.c $(pkg-config --cflags --libs ghcomp) -o my_code
```

## Project Structure

```
cbits/
  ghc_omp_runtime_rts.c    # The OpenMP runtime (~1300 lines)
  ghc_omp_runtime.c         # Phase 1 reference stub (pthread-based)
  omp_compute.c              # Shared compute kernels (sinsum, dgemm, etc.)
  omp_prims.cmm              # Cmm primitives (zero-overhead RTS access)
  omp_batch.cmm              # Batched safe calls (manual suspend/resume)
  HsStub.hs                  # Minimal Haskell module for RTS initialization
  bench_overhead.c           # Microbenchmark suite
  bench_dgemm.c              # DGEMM benchmark (native vs RTS)
  test_*.c                   # C test programs

demos/
  HsMain.hs                  # Haskell FFI interop demo
  HsConcurrent.hs            # Concurrent Haskell + OpenMP
  HsGCStress.hs              # GC interaction test
  HsMatMul.hs                # Dense matrix multiply
  HsCallback.hs              # Bidirectional interop (OpenMP -> Haskell)
  HsCmmDemo.hs               # Calling convention benchmark
  HsCmmBatch.hs              # Batch overhead benchmark
  HsCrossover.hs             # Parallelism crossover analysis
  HsParCompare.hs            # GHC forkIO vs OpenMP comparison
  HsTaskDemo.hs              # Deferred task execution
  HsZeroCopy.hs              # Zero-copy FFI with pinned ByteArray
  HsLinearDemo.hs            # Linear typed arrays demo
  Data/Array/Linear.hs       # Linear typed array library
  inline-cmm/                # inline-cmm quasiquoter demo (separate cabal package)

lib/
  GHC/OpenMP.hs              # Haskell API (Haddock: jhhuh.github.io/ghc-openmp/haddock/)
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

<!-- BENCH:readme_micro -->
| Metric | Native libgomp | RTS-backed | Ratio |
|--------|---------------|------------|-------|
| Fork/join | 0.700 us | 0.601 us | **0.86x** (RTS faster) |
| Barrier | 0.186 us | 0.181 us | 0.97x (parity) |
| Parallel for (1M sin) | 2.881 ms | 3.226 ms | 1.12x |
| Critical section | 0.248 ms | 0.326 ms | 1.31x |
<!-- /BENCH:readme_micro -->

### DGEMM (dense matrix multiply)

<!-- BENCH:readme_dgemm -->
| N | Native (ms) | RTS (ms) | Ratio |
|---|------------|---------|-------|
| 512 | 66.11 | 68.05 | 1.03x |
| 1024 | 603.84 | 536.88 | 0.89x |
<!-- /BENCH:readme_dgemm -->

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

[Cmm](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/cmm-type)
(GHC's low-level intermediate representation) primitives callable from
Haskell via `foreign import prim` — the fastest calling convention GHC
offers. Arguments pass directly in STG registers with no FFI boundary at all.

For example, reading the current Capability number (equivalent to
`omp_get_thread_num()`) compiles to a single memory load:

```c
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

The ~68ns safe FFI overhead can be amortized by batching multiple C calls
within a single `suspendThread`/`resumeThread` cycle, written manually in Cmm:

| Batch size | Per-call cost | Speedup vs safe |
|---|---|---|
| 1 | 69 ns | 1.0x |
| 10 | 8.7 ns | 8.2x |
| 100 | 2.7 ns | 27x |

At batch=100, per-call overhead approaches unsafe FFI cost (~2 ns).

## License

BSD-3-Clause. See [LICENSE](LICENSE).
