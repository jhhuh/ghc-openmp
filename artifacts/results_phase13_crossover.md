# Phase 13: Parallelism Crossover Analysis — Results

## Question

When calling OpenMP from Haskell via safe FFI, how large must the workload
be before parallelism pays for itself?

## Fixed Overheads (measured)

| Layer | Cost |
|---|---|
| Safe FFI (suspendThread/resumeThread) | ~86 ns |
| OpenMP fork/join (barrier + generation) | ~1712 ns |
| Total fixed overhead per parallel call | ~1800 ns |

## Crossover Data (4 threads, i7-10750H)

| Elements | Seq unsafe | Seq safe | Par safe | Speedup |
|---|---|---|---|---|
| 10 | 0.1 us | 0.2 us | 2.0 us | 0.04x |
| 50 | 0.3 us | 0.4 us | 2.0 us | 0.14x |
| 100 | 0.5 us | 0.6 us | 2.1 us | 0.25x |
| 200 | 1.3 us | 1.3 us | 2.2 us | 0.56x |
| **500** | **3.6 us** | **3.7 us** | **2.9 us** | **1.22x** |
| 1000 | 7.5 us | 7.6 us | 3.9 us | 1.94x |
| 5000 | 49.0 us | 49.1 us | 16.6 us | 2.95x |
| 10000 | 105.8 us | 105.9 us | 30.6 us | 3.46x |
| 100000 | 1131.9 us | 1101.6 us | 278.6 us | 4.06x |
| 1000000 | 11236.3 us | 11074.0 us | 2841.3 us | 3.95x |

## Key Findings

1. **Crossover at ~500 elements**: For compute-bound work (~11ns/element),
   OpenMP from Haskell breaks even at ~500 elements and wins convincingly
   above 1000.

2. **Safe FFI overhead is negligible**: The 86ns safe FFI tax is dwarfed by
   the 1.7us OpenMP fork/join overhead. Using `foreign import ccall safe`
   adds <5% to the total parallel region cost.

3. **Near-ideal scaling**: At large sizes (100K+), speedup approaches 4x
   on 4 threads (~4.06x at 100K). The runtime achieves near-linear scaling.

4. **Unsafe vs safe FFI parity**: For workloads above 100 elements,
   `ccall safe` and `ccall unsafe` have identical performance (within noise).
   Below 100 elements, the 86ns difference matters.

## Crossover Formula

```
crossover_elements = fork_join_overhead / ((1 - 1/N) * per_element_cost)
```

With N=4 threads:
- sin() at ~11ns/element: crossover = 1712 / (0.75 * 11) ≈ 207 elements
- Actual measurement: ~350-500 elements (barrier + cache effects add overhead)

## Practical Guideline

For Haskell programs calling OpenMP C code:
- **< 200 elements**: Use `foreign import ccall unsafe` (sequential C)
- **200-1000 elements**: OpenMP may or may not win (depends on per-element cost)
- **> 1000 elements**: OpenMP wins clearly, use `foreign import ccall safe`
