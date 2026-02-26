## A.6 Zero-Copy FFI with Pinned ByteArray

*Source: [`HsZeroCopy.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsZeroCopy.hs)*

The standard FFI pattern (`allocaArray` + `peekElemOff`/`pokeElemOff`) boxes
every element as `CDouble` and converts via `realToFrac`, adding overhead at
the Haskell↔OpenMP boundary:

```haskell
-- Boxed: every element goes through CDouble
a <- peekElemOff pA (i * n + k)    -- returns boxed CDouble
b <- peekElemOff pB (k * n + j)    -- returns boxed CDouble
go (acc + realToFrac a * realToFrac b) (k + 1)  -- 4 box/unbox ops
```

Using pinned `ByteArray#` with unboxed primops eliminates this overhead:

```haskell
-- Unboxed: Double# throughout, no boxing
case readDoubleArray# mbaA (i *# n +# k) s of
    (# s', a #) ->     -- a :: Double# (unboxed)
        case readDoubleArray# mbaB (k *# n +# j) s' of
            (# s'', b #) ->     -- b :: Double# (unboxed)
                goK s'' i j (k +# 1#) (acc +## (a *## b))
```

The pinned ByteArray is passed to C via `mutableByteArrayContents#`, which
returns a raw `Addr#` — zero-copy, no marshalling. `touch#` keeps the
ByteArray alive during the C call.

### Benchmark

Haskell sequential DGEMM inner loop, pinned ByteArray with unboxed primops
vs standard boxed FFI:

| N | Boxed (ms) | Unboxed (ms) | Speedup |
|--:|-----------:|-------------:|--------:|
| 256 | 57.3 | 53.4 | 1.07x |
| 512 | 457.9 | 384.6 | **1.19x** |

The 19% improvement at N=512 comes from eliminating `CDouble` boxing in the
O(n³) inner loop. `-ddump-simpl` confirms the hot loop uses `+##`, `*##`, and
`readDoubleArray#` with no `D#` constructor.

---

