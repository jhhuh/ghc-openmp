# Plan: Phase 3 — Synchronization Optimizations

## Problem
Phase 2 benchmarks show fork/join is ~24x slower and barriers ~14x slower
than native libgomp. Both bottlenecks stem from using mutex+condvar for
all synchronization.

## Optimization Strategy

### 1. Spin-then-block for worker wakeup
Replace `pthread_cond_wait` in worker loop with:
- Spin on atomic generation counter for ~1000 iterations
- Fall through to `futex_wait` if still no work
- Master uses `futex_wake` instead of `pthread_cond_broadcast`

Expected impact: eliminates condvar overhead for back-to-back parallel regions.

### 2. Atomic generation counter (lock-free work dispatch)
Replace mutex-protected `g_pool.generation` with:
- `atomic_int` generation counter
- Workers spin-read the counter (no mutex needed to detect new work)
- Master atomically stores fn/data/num_threads, then increments generation
- Eliminates mutex lock/unlock from the hot path entirely

### 3. Sense-reversing centralized barrier
Replace `pthread_barrier_t` with a custom atomic barrier:
- Shared atomic counter + sense flag
- Threads atomically decrement counter; last thread flips sense
- No mutex, no condvar — pure atomic operations
- Use for both start_barrier and end_barrier

### 4. Completion signaling with atomics
Replace mutex-protected `workers_done` counter with:
- `atomic_int` workers_done
- Master spins on it (with optional futex fallback)
- Eliminates the work_done condvar entirely

## Implementation Order
1. Atomic generation + spin-wait (biggest win: eliminates mutex from hot path)
2. Sense-reversing barrier (replaces pthread_barrier)
3. Atomic completion counter (replaces work_done condvar)
4. Optional: futex fallback for power-efficiency on idle wait
