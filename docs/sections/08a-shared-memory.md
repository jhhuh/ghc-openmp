## Shared Memory Demos

*Source: [`HsSharedMem1.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsSharedMem1.hs), [`HsSharedMem2.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsSharedMem2.hs), [`HsSharedMem3.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsSharedMem3.hs), [`omp_shared.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_shared.c)*

Three demos show Haskell and OpenMP C code operating on the same pinned
ByteArray, with increasing type safety. The progression demonstrates that
linear types can eliminate defensive synchronization while providing
compile-time proof of safe concurrent access.

All three use the same workload: element-wise
`f(x) = sin(x) * cos(x) + sqrt(|x|)`, applied by both Haskell and C/OpenMP
to portions of a shared array.

#### Demo 1: Producer-Consumer

Sequential handoff — Haskell fills, C transforms, Haskell reads:

```haskell
arrIn  <- newPinnedDoubles n     -- pinned: not moved by GC
arrOut <- newPinnedDoubles n
forM_ [0..n-1] $ \i -> writeD arrIn i (fromIntegral i * 0.001)
c_transform_all (ptrOf arrIn) (ptrOf arrOut) (fromIntegral n)
s <- sumArray arrOut n           -- read back in Haskell
```

No concurrent access, no synchronization. Establishes the basic zero-copy
sharing pattern via `mutableByteArrayContents#`.

#### Demo 2: Synchronized Concurrent Access

Haskell and C/OpenMP each process a disjoint half of the output array.
Without compile-time proof of disjointness, a `GOMP_barrier()` is needed
for memory visibility — even though the regions never actually overlap:

```haskell
hsTransformRange arrIn arrOut 0 half                 -- Haskell: [0, half)
c_transform_range_barrier pIn pOut half (n - half)   -- C: [half, n) + barrier
```

This "defensive synchronization" is the cost of not having type-level
guarantees about disjoint access.

#### Demo 3: Linear Concurrent Access

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

#### Results (4 threads, i7-10750H)

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

The modest 1–4% improvement reflects that GOMP_barrier latency (~0.3 us at
4 threads) is small relative to the compute-heavy transform. The primary
value is **correctness**: the type checker rejects programs that access
overlapping regions, preventing data races at compile time rather than
requiring manual reasoning about disjointness. The zero-cost property of
`split`/`combine` means this safety comes with no runtime overhead.

---

