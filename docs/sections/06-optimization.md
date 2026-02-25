## 5. Optimization: From 24x Slower to Parity

The Phase 2 runtime was functional but slow: fork/join took 24 us vs
libgomp's 1 us. The bottleneck was mutex+condvar on every operation. We
eliminated all locks from the hot path:

### 5.1 Lock-free Work Dispatch

```c
// Master: store work, then release-fence generation increment
g_pool.fn = fn;
g_pool.data = data;
atomic_fetch_add(&g_pool.generation, 1, memory_order_release);

// Worker: spin on generation (acquire-fence)
while (atomic_load(&g_pool.generation, memory_order_acquire) == my_gen)
    _mm_pause();
```

No mutex on the hot path. The condvar broadcast is only for workers that
fell asleep after 4000 spin iterations.

### 5.2 Sense-Reversing Barrier

The sense-reversing centralized barrier follows Mellor-Crummey & Scott's
algorithm (*"Algorithms for Scalable Synchronization on Shared-Memory
Multiprocessors"*, ACM TOCS 9(1), 1991):

```c
void spin_barrier_wait(spin_barrier_t *b, int *local_sense) {
    *local_sense = 1 - *local_sense;
    if (atomic_fetch_sub(&b->count, 1, memory_order_acq_rel) == 1) {
        // Last thread: reset counter, flip global sense
        atomic_store(&b->count, b->size, memory_order_relaxed);
        atomic_store(&b->sense, *local_sense, memory_order_release);
    } else {
        // Spin until sense matches
        while (atomic_load(&b->sense, memory_order_acquire) != *local_sense)
            _mm_pause();
    }
}
```

Pure atomic operations, no locks. The centralized design has O(N) wakeup but
is optimal for small team sizes (typical OpenMP use).

### 5.3 Results

| Metric (4 threads) | Phase 2 | Phase 3 | Native libgomp |
|---|---|---|---|
| Fork/join | 24.35 us | 0.81 us | 0.97 us |
| Barrier | 7.01 us | 0.25 us | 0.51 us |
| Parallel for (1M sin) | 6.71 ms | 3.91 ms | 3.85 ms |
| Critical section | 0.39 ms | 0.38 ms | 0.92 ms |

After optimization, the RTS-backed runtime **matches or beats**
native libgomp on all benchmarks.

<figure>
<img src="charts/optimization-journey.svg" alt="Optimization Journey: Phase 2 → Phase 3" />
</figure>

---

