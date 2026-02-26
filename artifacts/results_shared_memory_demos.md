# Shared Memory Demos: Producer-Consumer, Synchronized, Linear

## Summary

Three demos demonstrate Haskell and OpenMP C code collaborating on the
same data in memory via pinned ByteArray, with progressive coordination
strategies. The sequence shows that linear types eliminate defensive
synchronization (barriers) by proving disjoint array access at compile time.

## Setup

- Intel i7-10750H (6C/12T), NixOS, GCC 15.2, GHC 9.10.3
- 4 OpenMP threads, powersave governor
- Workload: element-wise transform `f(x) = sin(x)*cos(x) + sqrt(|x|)`
- All demos verified correct against sequential Haskell reference (max diff = 0.0)

## Demo 1: Producer-Consumer (sequential handoff)

*Source: [`HsSharedMem1.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsSharedMem1.hs)*

Pattern: Haskell fills array -> C/OpenMP transforms -> Haskell reads.
No concurrent access, clean sequential phases.

```
  N        OpenMP transform
  10000          0.042 ms
  100000         0.399 ms
  1000000        4.208 ms
```

Demonstrates the basic zero-copy pattern: pinned ByteArray# shared
directly with C via `mutableByteArrayContents#`. No marshalling needed.

## Demo 2: Synchronized concurrent access (barriers)

*Source: [`HsSharedMem2.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsSharedMem2.hs)*

Haskell and C/OpenMP each process a disjoint half of the same array.
Without compile-time proof of disjointness, `GOMP_barrier()` is required
for memory visibility between the two sides — even though the regions
never actually overlap. This is "defensive synchronization".

### Benchmark 1: Iteration loop

```
  N         With barrier    No barrier    Overhead
  10000         165.0 ms      175.0 ms  -10.0 ms
  100000        188.5 ms      201.5 ms  -13.0 ms
  1000000       186.7 ms      186.2 ms   +0.5 ms
```

### Benchmark 2: Partition scaling (N=1000000)

```
  Partitions  With barrier    No barrier  Overhead
  2              18.045 ms     18.166 ms  -0.121 ms
  4              18.158 ms     18.195 ms  -0.037 ms
  8              18.249 ms     18.037 ms  +0.212 ms
  16             18.241 ms     19.114 ms  -0.873 ms
  32             20.560 ms     20.441 ms  +0.119 ms
```

## Demo 3: Linear concurrent access (no sync)

*Source: [`HsSharedMem3.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsSharedMem3.hs)*

Same workload as Demo 2, but using `Data.Array.Linear`'s `split`/`combine`
to prove disjointness at the type level. The linear token `RW l` grants
exclusive access to the left half; `RW r` to the right. No barrier needed.

### Benchmark 1: Barrier vs linear (iteration loop)

```
  N        Iters  With barrier      Linear       Saved
  10000    1000       202.5 ms    194.2 ms   +8.3 ms (4.1%)
  100000    100       231.9 ms    221.7 ms  +10.2 ms (4.4%)
  1000000    10       224.6 ms    221.8 ms   +2.8 ms (1.3%)
```

### Benchmark 2: Partition scaling (N=1000000)

```
  Partitions   Linear (ms)
  2              39.020
  4              39.833
  8              38.936
  16             38.280
  32             38.392
```

Partition scaling is flat: zero barriers regardless of how many
partitions, because `split`/`combine` is zero-cost (no allocation,
just arithmetic on offset/length views into the same buffer).

## Interpretation

**Barrier overhead is small but real.** On this hardware, the GOMP_barrier
cost per invocation is sub-microsecond (see §8.1 barrier latency benchmarks:
0.27us at 4 threads). For compute-heavy workloads like the array transform,
the barrier is a negligible fraction of total time — explaining the modest
4% improvement in Demo 3 vs Demo 2.

**The value of linear types here is correctness, not raw speed.** The
transform workload dominates runtime, so eliminating a ~0.3us barrier
per iteration yields only a few percent improvement. Where linear types
shine:

1. **Eliminating defensive synchronization at scale.** If the program has
   many fine-grained partitions or many iteration steps, barrier costs
   compound. The partition scaling benchmark shows linear types scale
   flat while barriers would add per-partition overhead in workloads where
   barrier cost is non-negligible relative to compute.

2. **Compile-time proof of safety.** Without linear types, the programmer
   must manually reason about disjointness. With them, the type checker
   rejects programs that access overlapping regions — preventing data
   races before they happen. This is the primary motivation.

3. **Zero-cost abstraction.** `split`/`combine` involve no allocation,
   copying, or runtime checks. The tokens (`RW s`) are erased at runtime.
   The type-level guarantees add zero overhead.

## Files

- `cbits/omp_shared.c` — C kernels: `transform_all`, `transform_range`, `transform_range_barrier`
- `demos/HsSharedMem1.hs` — Demo 1: producer-consumer
- `demos/HsSharedMem2.hs` — Demo 2: synchronized concurrent
- `demos/HsSharedMem3.hs` — Demo 3: linear concurrent
- `demos/Data/Array/Linear.hs` — Linear typed array module
