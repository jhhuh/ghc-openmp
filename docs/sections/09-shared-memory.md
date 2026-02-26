## 8. Shared Memory Demos

*Source: [`HsSharedMem1.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsSharedMem1.hs), [`HsSharedMem2.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsSharedMem2.hs), [`HsSharedMem3.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsSharedMem3.hs), [`HsSharedMem4.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsSharedMem4.hs), [`HsSharedMem5.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsSharedMem5.hs), [`omp_shared.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_shared.c)*

Everything in this project builds to this: Haskell and OpenMP C code
operating on the same data, concurrently, with type-level proof that
their access patterns are safe. The unified runtime (§6) makes shared
memory possible; zero-copy FFI (§A.6) makes it practical; linear types
(§A.7) make it correct by construction.

Five demos show this progression — from sequential handoff, through
defensive synchronization, to compile-time proof of disjoint access,
safety guarantees, and finally pure Haskell parallelism via GHC sparks.
All use the same workload: element-wise
`f(x) = sin(x) * cos(x) + sqrt(|x|)`, applied by both Haskell and C/OpenMP
to portions of a shared array.

#### Demo 1: The basic pattern works

Sequential handoff — Haskell fills, C transforms, Haskell reads:

```haskell
arrIn  <- newPinnedDoubles n     -- pinned: not moved by GC
arrOut <- newPinnedDoubles n
forM_ [0..n-1] $ \i -> writeD arrIn i (fromIntegral i * 0.001)
c_transform_all (ptrOf arrIn) (ptrOf arrOut) (fromIntegral n)
s <- sumArray arrOut n           -- read back in Haskell
```

No concurrent access, no synchronization. This establishes that the
plumbing works: a pinned ByteArray created in Haskell is directly
readable and writable by C/OpenMP code via `mutableByteArrayContents#`.

#### Demo 2: The problem — defensive synchronization

Haskell and C/OpenMP each process a disjoint half of the output array.
Without compile-time proof of disjointness, a `GOMP_barrier()` is needed
for memory visibility — even though the regions never actually overlap:

```haskell
hsTransformRange arrIn arrOut 0 half                 -- Haskell: [0, half)
c_transform_range_barrier pIn pOut half (n - half)   -- C: [half, n) + barrier
```

This "defensive synchronization" is the cost of not having type-level
guarantees about disjoint access. The barrier is cheap (~0.3 us), but
it represents a fundamental limitation: the programmer must manually
reason about which regions are disjoint, and the compiler cannot help.

#### Demo 3: The solution — linear types eliminate barriers

Same partition, but using `Data.Array.Linear`'s `split`/`combine` to prove
disjointness at the type level:

```haskell
case halve rw arrOut of
    MkSlice st rwL rwR arrL arrR ->
        let rwL' = linearTransform rwL arrIn arrL      -- Haskell: left half
            !()  = unsafePerformIO $                    -- C: right half
                unsafeWithPtr arrOut $ \pOut ->
                    unsafeWithPtr arrIn $ \pIn ->
                        c_transform_range pIn pOut
                            (fromIntegral half) (fromIntegral (n - half))
        in combine st rwL' rwR                         -- no barrier
```

The type system enforces:

1. `rwL` grants exclusive write access to the left half
2. `rwR` grants exclusive access to the right half — consumed by `combine`
3. No Haskell code can use `rwR` to access `arrR` while C is processing it
4. `combine` is zero-cost (no allocation, no copying)

#### Demo 4: Safety — why barriers exist

Two examples showing that removing barriers (`nowait`) without proof of
disjointness silently introduces bugs.

**Part A — Off-by-one overlap.** Each partition writes `[off..off+chunk+1)`
instead of `[off..off+chunk)`. Boundary elements are written by two
partitions. With `nowait`, this is a data race; with barriers, it is
deterministic but still wrong (double-write at boundaries). With linear
`split`, overlapping ranges are impossible — the type system rejects them:

| Variant | N=10K, P=4 | N=100K, P=16 | N=1M, P=64 |
|---|---|---|---|
| Disjoint (correct) | 0.00 | 0.00 | 0.00 |
| Overlap + barrier | 3.06 | 9.26 | 31.8 |
| Overlap + nowait | 3.06 | 9.26 | 31.8 |

**Part B — Two-pass stencil.** Pass 1 writes `out[i] = f(in[i])`
(independent per element); pass 2 reads neighbors
`out[i] = avg(out[i-1..i+1])` (crosses partition boundaries). Without
a barrier between passes, pass 2 reads stale data. With linear types,
`combine` forces pass 1 to complete before pass 2 can access the parent
token needed for cross-boundary reads:

```haskell
let rw1 = linearMultiPass1 rw arrIn arrOut 0 numParts  -- partitioned pass 1
    -- combine happened inside — rw1 is the parent token
in  linearPass2 rw1 arrOut                              -- pass 2 needs parent
```

| Variant | N=10K | N=100K | N=1M |
|---|---|---|---|
| C nowait (worst of 10 runs) | 2.68e-2 | 1.09e-2 | 4.44e-3 |
| Haskell linear vs ref | 0.00 | 0.00 | 0.00 |

The C `nowait` version produces wrong results (stale reads at partition
boundaries). The Haskell linear version is both correct and barrier-free.

#### Demo 5: GHC spark parallelism

Demos 2–4 use C/OpenMP for the parallel half. Demo 5 shows that the same
`split`/`combine` pattern works for pure Haskell parallelism via GHC sparks.
`parCombine` replaces `combine` — it sparks the left partition and evaluates
the right on the current thread, using `spark#`/`seq#` with `noDuplicate#`
for safety:

```haskell
parPartition rw arrIn arrOut base depth =
    case halve rw arrOut of
        MkSlice st rwL rwR arrL arrR ->
            let rwL' = parPartition rwL arrIn arrL base (depth - 1)
                rwR' = parPartition rwR arrIn arrR (base + size arrL) (depth - 1)
            in parCombine st rwL' rwR'   -- spark left, eval right
```

**Spark scaling (N=1,000,000, 4 capabilities):**

| Depth | Partitions | Sequential | Parallel | Speedup |
|---|---|---|---|---|
| 0 | 1 | 41.9 ms | 43.6 ms | 0.96x |
| 1 | 2 | 44.4 ms | 22.3 ms | 1.99x |
| 2 | 4 | 42.5 ms | 13.8 ms | 3.09x |
| 3 | 8 | 42.1 ms | 15.0 ms | 2.80x |
| 5 | 32 | 46.1 ms | 13.5 ms | 3.41x |
| 6 | 64 | 45.7 ms | 13.7 ms | 3.34x |

Near-ideal 2x at 2 partitions, ~3x at 4+ partitions on 4 cores.
No C/OpenMP involved — parallelism is GHC's work-stealing scheduler.

#### Results (Demos 1–3, 4 threads, i7-10750H)

**Barrier vs linear — iteration loop:**

| N | Iters | With barrier | Linear | Saved |
|---|---|---|---|---|
| 10,000 | 1000 | 202.5 ms | 194.2 ms | 4.1% |
| 100,000 | 100 | 231.9 ms | 221.7 ms | 4.4% |
| 1,000,000 | 10 | 224.6 ms | 221.8 ms | 1.3% |

**Partition scaling (N=1,000,000) — linear types:**

| Partitions | Time (ms) |
|---|---|
| 2 | 39.0 |
| 4 | 39.8 |
| 8 | 38.9 |
| 16 | 38.3 |
| 32 | 38.4 |

Scaling is flat: zero barriers regardless of partition count, because
`split`/`combine` is pure arithmetic on offset/length views into the same
underlying buffer.

#### Interpretation

The modest 1–4% improvement in Demo 3 vs Demo 2 confirms that barrier
elimination is not the point — the barrier was already fast. The point is
**correctness and composability**:

1. **Safety (Demo 4)**: The type checker rejects programs that access
   overlapping regions (Part A) and enforces ordering between passes that
   share data across partition boundaries (Part B). These are real bugs
   that `nowait` introduces silently — linear types catch them at compile
   time.

2. **Zero-cost abstraction**: `split`/`combine` involves no allocation,
   copying, or runtime checks. The tokens (`RW s`) are erased at runtime.
   The generated code is identical to the unsafe version minus the barrier.

3. **Composable parallelism (Demo 5)**: The same `split`/`combine` pattern
   works with GHC sparks (`parCombine`) for pure Haskell parallelism,
   achieving near-ideal scaling. The programmer writes the same code
   regardless of whether parallelism comes from OpenMP (C FFI) or GHC
   sparks — linear types guarantee safety in both cases.

---

