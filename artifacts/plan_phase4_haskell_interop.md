# Plan: Phase 4 — Haskell ↔ OpenMP Interop Demo

## Goal
Demonstrate the core value proposition: a Haskell program calls OpenMP-parallelized
C code, both sharing GHC RTS Capabilities as the thread pool.

## Architecture

```
Haskell main → FFI safe call → C function with #pragma omp parallel for
                                  ↓
                              GOMP_parallel (our runtime)
                                  ↓
                              Worker pool (OS threads, same as RTS Capabilities)
```

When Haskell calls C via FFI:
1. GHC releases the calling thread's Capability (safe foreign call)
2. C code enters GOMP_parallel → our runtime dispatches to worker pool
3. Workers are OS threads registered with GHC but NOT holding Capabilities
4. After OpenMP region completes, control returns to Haskell

Key insight: `hs_init_ghc()` is reference-counted — calling it when RTS is
already running (Haskell host) just increments the counter and returns. Our
runtime auto-detects the existing RTS and uses its Capabilities.

## Components

### 1. C library: `src/omp_compute.c`
OpenMP-parallelized compute functions:
- `parallel_dot(double *a, double *b, int n)` — dot product with reduction
- `parallel_saxpy(double alpha, double *x, double *y, int n)` — SAXPY

### 2. Haskell driver: `src/HsMain.hs`
- Allocates arrays via `Foreign.Marshal`
- Calls C functions via `foreign import ccall`
- Verifies correctness against pure Haskell computation
- Times the FFI call

### 3. Build
```
gcc -fopenmp -O2 -c omp_compute.c → omp_compute.o
ghc -threaded -O2 HsMain.hs omp_compute.o ghc_omp_runtime_rts.o -o demo
```

## Runtime Init Considerations
- Haskell provides `main` → GHC boots RTS before any C runs
- `ensure_rts() → init_rts()` calls `hs_init_ghc()` which returns immediately
  (ref count bump, no re-init)
- `getNumCapabilities()` returns GHC's existing cap count
- Worker pool spawns and registers with existing RTS
- `shutdown_rts()` calls `hs_exit()` which just decrements ref count

## Success Criteria
1. Haskell program runs, calls OpenMP C code, gets correct results
2. Speedup scales with +RTS -N (e.g., -N1 vs -N4)
3. No crashes, no RTS assertion failures
