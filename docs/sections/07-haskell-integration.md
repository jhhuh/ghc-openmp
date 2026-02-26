## Haskell Integration {#sec:haskell-integration}

This section covers the integration between Haskell and the OpenMP runtime:
calling conventions, initialization, concurrent execution, garbage collection
behavior, and bidirectional callbacks.

See also the [Haddock API reference](haddock/) for the `GHC.OpenMP` module.

### FFI Calling Convention {#sec:ffi-calling-convention}

*Source: [`HsMain.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsMain.hs)*

Haskell calls OpenMP C code via `foreign import ccall safe`:

```haskell
foreign import ccall safe "parallel_sinsum"
    c_parallel_sinsum :: CInt -> IO CDouble
```

The `safe` keyword is critical: it tells GHC to release the
calling Capability before entering the foreign code, and reacquire it on
return. This means:

- Other Haskell green threads can run on the released Capability
- The C code enters `GOMP_parallel`, which dispatches to the
  worker pool — including potentially the Capability just released
- No deadlock: workers don't need to hold Capabilities to execute C
  compute kernels

### RTS Initialization {#sec:rts-init}

When called from a Haskell host, `hs_init_ghc()` is already done
by GHC before `main`. Our runtime's `ensure_rts()` calls
`hs_init_ghc()` again, which simply increments the reference count
and returns. The runtime discovers the existing Capabilities via
`getNumCapabilities()` and spawns workers for Caps 1..N-1.

### Concurrent Execution {#sec:concurrent-execution}

*Source: [`HsConcurrent.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsConcurrent.hs)*

```haskell
-- Haskell green thread: pure computation
_ <- forkIO $ do
    let !result = haskellSinSum 1200000
    putMVar hsDone result

-- OpenMP FFI call (safe: releases Capability)
_ <- forkIO $ do
    result <- c_parallel_sinsum 12000000
    putMVar ompDone result

-- Both run simultaneously!
```

Measured: sequential 68ms → concurrent 58ms, with 10ms of overlapping
execution confirmed.

### Garbage Collection Isolation {#sec:gc-isolation}

*Source: [`HsGCStress.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsGCStress.hs)*

A key concern: GHC's stop-the-world GC pauses all threads holding
Capabilities. Would this stall OpenMP workers?

> **Answer: No.** OpenMP workers do not hold Capabilities during
> parallel execution. After their initial
> `rts_lock()/rts_unlock()` registration, they are plain OS threads
> spinning on atomic variables. GC only synchronizes Capability-holding threads
> — our workers are invisible.

#### Experimental Validation

We ran 500 OpenMP parallel regions (each ~400us) concurrently with:

| Scenario | p50 (us) | p99 (us) | max (us) |
|---|---|---|---|
| Baseline (OpenMP alone) | 314–478 | 636–658 | 692–783 |
| + allocation pressure (50K rounds) | 313–543 | 538–651 | 585–691 |
| + forced major GC (20 × performGC) | 315–556 | 549–744 | 574–2262 |

Allocation pressure has negligible impact (within noise). Forced major GCs
produced one outlier spike of 2262us on one run and none on another. The spike
correlates with the GHC RTS reporting a 1.6ms max GC pause — likely the
OS thread making the FFI call had its Capability briefly paused at a region
boundary.

GHC RTS statistics: 99.7% productivity, GC time <0.5% of elapsed.

### Bidirectional Callbacks {#sec:bidirectional-callbacks}

*Source: [`HsCallback.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCallback.hs), [`omp_compute.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_compute.c)*

The previous sections demonstrated Haskell calling OpenMP. **OpenMP workers
can also call back into Haskell** from within a parallel region.

#### Mechanism

Haskell creates a `FunPtr` via
`foreign import ccall "wrapper"`:

```haskell
foreign import ccall "wrapper"
    mkCallback :: (CInt -> IO CDouble)
               -> IO (FunPtr (CInt -> IO CDouble))

sinCb <- mkCallback (\i -> return (sin (fromIntegral i * 0.001)))
```

GHC generates a C stub that wraps the Haskell closure with automatic
Capability management:

```c
// Generated wrapper (simplified):
CDouble wrapper(CInt arg) {
    Capability *cap = rts_lock();      // acquire Capability
    // ... evaluate Haskell closure ...
    rts_unlock(cap);                   // release Capability
    return result;
}
```

The C code calls this `FunPtr` from inside an OpenMP parallel for:

```c
void parallel_reduce_callback(hs_callback_t callback, int n) {
    double sum = 0.0;
    #pragma omp parallel for reduction(+:sum) schedule(static)
    for (int i = 0; i < n; i++)
        sum += callback(i);  // each worker calls into Haskell
    return sum;
}
```

#### Correctness

All results verified against pure C and pure Haskell reference
implementations:

| Test | Result | Status |
|---|---|---|
| parallel_map (1000 sin values) | Element-wise match to 1e-10 | OK |
| parallel_reduce (100K sin sum) | 1839.343386 (matches pure C) | OK |
| polynomial callback (10K) | 1109840.005000 (matches Haskell) | OK |

#### Performance

| Threads | Pure C (ms) | Callback (ms) | Overhead | Per-callback |
|---|---|---|---|---|
| 1 | 1.69 | 46.60 | 27.6x | ~0.47 us |
| 2 | 1.17 | 60.43 | 51.8x | ~0.60 us |
| 4 | 0.71 | 57.91 | 82.1x | ~0.58 us |

The per-callback cost of ~0.5us is the `rts_lock()/rts_unlock()`
round-trip. This is constant regardless of what the Haskell function does.
For callbacks that perform milliseconds of work (e.g., looking up a Haskell
data structure, evaluating a complex expression), the overhead is negligible.
For tight inner loops like 100K trivial sin() calls, pure C should be used
instead.

> **Practical guideline**: Use Haskell callbacks when each
> invocation does ≥100us of work. Below that, the
> `rts_lock/unlock` overhead dominates. Structure code so that
> OpenMP handles the hot numerical loop in C, and calls Haskell for complex
> logic at coarser granularity.

---

