# Sense-reversing barrier: cross-thread reuse gotcha

## The gotcha

If a sense-reversing barrier is re-initialized (e.g., at the start of each
parallel region), the global sense resets to 0. But if threads retain their old
`local_sense` from a previous region, they fall through immediately.

## When this happens

`GOMP_parallel` called from different OS threads (e.g., Haskell `forkIO`).
Workers from a previous region have stale sense values.

## Fix

Reset ALL thread-local senses to match the barrier's initial sense at the
start of each parallel region, before dispatching any work.
