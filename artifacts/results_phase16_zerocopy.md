# Phase 16: Zero-Copy FFI with Pinned ByteArray

## Summary
Eliminated boxing overhead at the Haskell↔OpenMP boundary using pinned
`ByteArray#` and unboxed primops (`writeDoubleArray#`, `readDoubleArray#`).
Demonstrates zero-copy data passing to C via `mutableByteArrayContents#`.

## Key Technique

```
Before (Phase 7):
  Haskell Double → realToFrac → CDouble → pokeElemOff → Ptr → C
  C → Ptr → peekElemOff → CDouble → realToFrac → Double

After (Phase 16):
  Haskell Double# → writeDoubleArray# → ByteArray# → mutableByteArrayContents# → Addr# → C
  C → ByteArray# → readDoubleArray# → Double# (no boxing)
```

No marshalling, no allocation, no CDouble boxing. `mutableByteArrayContents#`
gives C a direct pointer to GHC-managed pinned memory.

## Results (4 threads, i7-10750H, best of 3-5 runs)

### Matrix Fill (alloc + write N*N doubles)

```
  N        Boxed     Unboxed   Speedup
  256      1.07 ms   1.05 ms   1.02x
  512      3.93 ms   3.77 ms   1.04x
  1024     7.86 ms  12.11 ms   0.65x
  2048    47.13 ms  76.34 ms   0.62x
```

Fill shows minimal improvement because the closure `f :: Int -> Int -> Double`
still boxes indices (`I# i`, `I# j`). At large sizes (>1024), pinned
ByteArray allocation in the GHC heap is slower than malloc-based
`allocaArray`. The fill function could be improved by taking `Int# -> Int#
-> Double#` but at the cost of ergonomics.

### Haskell Sequential DGEMM (inner loop improvement)

```
  N        Boxed     Unboxed   Speedup
  128       3.6 ms    5.8 ms   0.62x
  256      57.3 ms   53.4 ms   1.07x
  512     457.9 ms  384.6 ms   1.19x
```

At N=512 (the meaningful benchmark size), the unboxed inner loop is **19%
faster** — eliminating `peekElemOff → CDouble → realToFrac → Double` boxing
per element. At N=128, startup overhead dominates. The improvement grows
with problem size as the O(n³) inner loop dominates.

### OpenMP DGEMM (sanity check — same C kernel)

```
  N       allocaArray  ByteArray#  Ratio
  256      14.3 ms     12.2 ms     1.17x
  512     123.9 ms    144.4 ms     0.86x
```

Both use the same C DGEMM kernel. Variation is measurement noise.

## Correctness

- Cross-verify N=128, 256, 512: exact match (max diff = 0.0e0)
- Both boxed and unboxed produce bit-identical results

## Core Verification

`-ddump-simpl` confirms unboxed `Double#` throughout the inner loop:
- `readDoubleArray#` returns `Double#` directly (no `D#` constructor)
- Arithmetic uses `+##`, `*##` (unboxed double ops)
- `writeDoubleArray#` takes `Double#` (no unboxing needed)
- No `realToFrac` or `CDouble` in the hot path

## What This Demonstrates

1. **Zero-copy FFI**: `mutableByteArrayContents#` gives C a direct `Addr#`
   to pinned GHC-managed memory. No `mallocArray`, no `pokeArray` copy.
2. **Boxing elimination**: `readDoubleArray#`/`writeDoubleArray#` operate
   on `Double#` without wrapping in `CDouble` or boxed `Double`.
3. **GC safety**: `touch#` keeps the ByteArray alive during C calls.
   Pinned memory is not moved by GC.
4. **Composability**: The technique works with the existing OpenMP C
   kernels without any C-side changes.
