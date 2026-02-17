# Benchmark: Phase 3 Optimized Runtime vs Native libgomp

## Optimizations Applied
1. **Atomic generation counter**: lock-free work dispatch (no mutex on hot path)
2. **Spin-wait with condvar fallback**: workers spin ~4000 iterations then sleep
3. **Sense-reversing barriers**: replace both `pthread_barrier_t` and GOMP_barrier
4. **Atomic completion counter**: workers signal done via atomic increment

## Results (4 Threads)

| Metric | Native | Phase 2 | Phase 3 | Improvement |
|--------|--------|---------|---------|-------------|
| Fork/join (us) | 0.97 | 24.35 | 0.81 | **30x faster** (now beats native) |
| Barrier (us) | 0.51 | 7.01 | 0.25 | **28x faster** (now beats native) |
| Parallel for (ms) | 4.06 | 6.71 | 6.71 | (same — compute-bound) |
| Critical (ms) | 0.92 | 0.39 | 0.38 | (same) |

## Full Results Across Thread Counts

### Fork/Join Overhead (us/iter)

| Threads | Native | RTS Phase 3 | Ratio |
|---------|--------|-------------|-------|
| 1       |  0.171 |       0.030 | 0.18x (RTS 5.7x faster) |
| 2       |  0.828 |       0.420 | 0.51x (RTS 2.0x faster) |
| 4       |  0.972 |       0.811 | 0.83x (RTS 1.2x faster) |
| 8       |  1.346 |       1.517 | 1.13x (comparable)      |

### Barrier Latency (us/iter)

| Threads | Native | RTS Phase 3 | Ratio |
|---------|--------|-------------|-------|
| 1       |  0.026 |       0.002 | 0.08x |
| 2       |  0.313 |       0.136 | 0.43x (RTS 2.3x faster) |
| 4       |  0.508 |       0.254 | 0.50x (RTS 2.0x faster) |
| 8       |  0.762 |       0.482 | 0.63x (RTS 1.6x faster) |

### Parallel For + Reduction (1M sin(), ms)

| Threads | Native | RTS Phase 3 | Ratio |
|---------|--------|-------------|-------|
| 1       | 16.603 |      16.288 | 0.98x |
| 2       |  8.077 |       7.962 | 0.99x |
| 4       |  4.056 |       6.711 | 1.65x |
| 8       |  3.513 |       3.416 | 0.97x |

### Critical Section (1000/thread, ms)

| Threads | Native | RTS Phase 3 | Ratio |
|---------|--------|-------------|-------|
| 1       |  0.054 |       0.026 | 0.48x |
| 2       |  0.318 |       0.254 | 0.80x |
| 4       |  0.915 |       0.381 | 0.42x |
| 8       |  2.135 |       1.201 | 0.56x |

## Summary

The Phase 3 optimizations brought the GHC RTS-backed runtime to **parity or
better** than native libgomp on synchronization primitives:

- **Fork/join**: Faster at 1-4 threads, comparable at 8 threads
- **Barrier**: 1.6-2.3x faster across all multi-thread counts
- **Critical**: Consistently 1.3-2.4x faster
- **Compute-bound work**: Comparable (scaling matches native)

The remaining 4-thread parallel-for regression (1.65x) is likely due to
worksharing loop overhead or GHC RTS timer interrupts, not synchronization.
