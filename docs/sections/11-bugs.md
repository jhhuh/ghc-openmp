## 10. Notable Bugs and Fixes

### 10.1 Barrier Sense Mismatch Deadlock

**Symptom**: Program hangs when calling `GOMP_parallel`
from a `forkIO` thread at `-N4`. No output at all. Works
at `-N1`.

**Root cause**: Workers' local barrier sense variables
(`start_sense`, `end_sense`) persisted from previous
parallel regions (value 1), but `spin_barrier_init()` reset the
barrier's global sense to 0. On the next region:

- Workers flipped 1→0, saw `sense(0) == local_sense(0)`, passed through immediately
- Master (on a new OS thread from `forkIO`) had fresh sense=0, flipped to 1, but couldn't complete the barrier

**Fix**: Reset all local sense variables to 0 at the start of
each parallel region, matching the freshly initialized barriers.

### 10.2 False Parallel-For Regression

**Symptom**: At 4 threads, parallel for appeared 1.65x slower
than native libgomp (6.7ms vs 4.1ms).

**Root cause**: Single-sample measurement on a laptop with
`powersave` CPU governor (i7-10750H at 46% clock). CPU boost state
varied between process invocations.

**Fix**: Changed to best-of-10 within each process. Controlled
interleaved testing confirmed parity (3.85ms vs 3.91ms).

---

