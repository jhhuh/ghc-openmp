# Benchmark Results

Generated: 2026-02-25T15:02:15Z

**System**: Intel(R) Core(TM) i7-10750H CPU @ 2.60GHz
**Threads**: 4 | **GHC**: 9.10.3 | **GCC**: 15.2.0

## Microbenchmarks (4 threads)

| Metric | Native libgomp | RTS-backed | Ratio |
|--------|---------------|------------|-------|
| Fork/join | 0.700 us | 0.601 us | 0.86x |
| Barrier | 0.186 us | 0.181 us | 0.97x |
| Parallel for (1M sin) | 2.881 ms | 3.226 ms | 1.12x |
| Critical (1000/thread) | 0.248 ms | 0.326 ms | 1.31x |
| Tasks (10000) | 7.707 ms | 4.511 ms | 0.59x |

## DGEMM (4 threads)

| N | Native (ms) | RTS (ms) | Ratio |
|---|------------|---------|-------|
| 128 | 0.64 | 0.77 | 1.20x |
| 256 | 9.47 | 10.22 | 1.08x |
| 512 | 66.11 | 68.05 | 1.03x |
| 1024 | 603.84 | 536.88 | 0.89x |

## Calling Convention Overhead

| Convention | ns/call |
|---|---|
| foreign import prim (Cmm) | 0.0 |
| foreign import ccall unsafe | 2.3 |
| foreign import ccall safe | 66.6 |

## Batched Safe Calls

| Batch size | Per-call cost (ns) | Speedup vs safe |
|---|---|---|
| 1 | 68.9 | 1.0x |
| 2 | 35.9 | 2.0x |
| 5 | 15.4 | 4.6x |
| 10 | 8.8 | 8.0x |
| 20 | 5.3 | 13.5x |
| 50 | 3.4 | 20.9x |
| 100 | 2.7 | 26.5x |
