# GHC RTS as OpenMP worker pool

## Pattern

Use GHC Capabilities as the OpenMP thread pool. Workers are persistent OS
threads pinned to Capabilities. Communication via atomics, not mutexes.

## Architecture

C program / Haskell FFI → GOMP_parallel → ghc_omp_runtime_rts.c →
Workers pinned via rts_setInCallCapability() → Atomic generation counter
triggers work dispatch → Sense-reversing barriers for synchronization.

## Dispatch: generation counter (not mutex+condvar)

Mutex+condvar caused deadlocks:
1. Single-thread: master waits for `has_work=false`, no workers to set it.
2. Multi-thread: `cond_signal` wakes wrong thread → missed wakeup.

Fix: atomic generation counter. Master increments, workers spin until change.

## GC isolation

Workers don't hold Capabilities during spin-wait → invisible to GHC's
stop-the-world GC.
