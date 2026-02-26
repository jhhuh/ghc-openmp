# Benchmark Results

Generated: 2026-02-26T02:36:59Z

**System**: Intel(R) Core(TM) i7-10750H CPU @ 2.60GHz
**Threads**: 4 | **GHC**: 9.10.3 | **GCC**: 15.2.0

## Microbenchmarks (4 threads)

| Metric | Native libgomp | RTS-backed | Ratio |
|--------|---------------|------------|-------|
| Fork/join | 0.931 us | 0.945 us | 1.02x |
| Barrier | 0.248 us | 0.270 us | 1.09x |
| Parallel for (1M sin) | 3.777 ms | 3.879 ms | 1.03x |
| Critical (1000/thread) | 0.352 ms | 0.327 ms | 0.93x |
| Tasks (10000) | 10.400 ms | 6.066 ms | 0.58x |

## DGEMM (4 threads)

| N | Native (ms) | RTS (ms) | Ratio |
|---|------------|---------|-------|
| 128 | 0.93 | 1.05 | 1.13x |
| 256 | 10.83 | 13.60 | 1.26x |
| 512 | 78.59 | 83.66 | 1.06x |
| 1024 | 670.05 | 654.66 | 0.98x |

## Calling Convention Overhead

| Convention | ns/call |
|---|---|
| foreign import prim (Cmm) | 0.0 |
| foreign import ccall unsafe | 3.1 |
| foreign import ccall safe | 89.8 |

## Batched Safe Calls

| Batch size | Per-call cost (ns) | Speedup vs safe |
|---|---|---|
| 1 | 97.9 | 1.0x |
| 2 | 50.3 | 2.0x |
| 5 | 20.8 | 4.7x |
| 10 | 12.4 | 7.8x |
| 20 | 7.6 | 13.4x |
| 50 | 4.7 | 22.2x |
| 100 | 3.7 | 26.4x |
