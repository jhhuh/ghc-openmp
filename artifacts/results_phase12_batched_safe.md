# Phase 12: Batched Safe Calls via Cmm — Results

## Concept

Standard `foreign import ccall safe` does `suspendThread`/`resumeThread` per call (~72 ns).
By writing the suspend/resume manually in Cmm, we batch N calls in one cycle, amortizing the overhead.

**Theoretical per-call cost**: `(65 + N * 2) / N` ns

## Results (i7-10750H, 4 threads)

| Batch size | Standard safe | Cmm batched | Speedup |
|---|---|---|---|
| 1 | 72.4 ns | 69.1 ns | 1.0x |
| 2 | 71.3 ns | 36.1 ns | 2.0x |
| 5 | 73.0 ns | 15.2 ns | 4.8x |
| 10 | 71.0 ns | 8.7 ns | 8.2x |
| 20 | 71.1 ns | 5.3 ns | 13.5x |
| 50 | 71.7 ns | 3.4 ns | 21.1x |
| 100 | 71.4 ns | 2.7 ns | 26.8x |

## Validation

Results match theoretical predictions closely:
- Batch=1: ~69 ns (predicted ~67 ns) — matches standard safe FFI
- Batch=10: ~8.7 ns (predicted ~8.5 ns)
- Batch=100: ~2.7 ns (predicted ~2.6 ns) — approaches unsafe FFI cost (~2 ns)

## Key Implementation Details

### Cmm: Manual suspend/resume with thread state save

Critical: must save `Sp` to TSO before `suspendThread` and restore after `resumeThread`:

```cmm
stack = StgTSO_stackobj(CurrentTSO);
StgStack_sp(stack) = Sp;

(tok) = ccall suspendThread(BaseReg "ptr", 0);
// ... N calls to omp_get_thread_num() ...
(new_base) = ccall resumeThread(tok);
BaseReg = new_base;

stack = StgTSO_stackobj(CurrentTSO);
Sp = StgStack_sp(stack);
```

Without the Sp save/restore, the program segfaults because:
1. `suspendThread` releases the Capability, allowing GC to run
2. GC needs to scan the suspended thread's stack
3. Without a valid Sp saved in the TSO, GC scans garbage → crash

### Haskell: State# threading for correctness

`foreign import prim` is pure by default. Using `State# RealWorld` threading
prevents GHC from CSE/LICM optimizing away the suspend/resume side effects:

```haskell
foreign import prim "cmm_batched_tid"
    cmmBatchedTid# :: Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
```

At the Cmm level, `State#` is erased — the function still takes/returns one `W_` each.
But GHC's optimizer respects the `State#` data dependency, preserving call ordering.

### Bug fix: "ptr" annotation on resumeThread token

The `tok` from `suspendThread` is `void*` (opaque token), NOT a GC-traceable pointer.
Using `resumeThread(tok "ptr")` incorrectly tells GHC's Cmm compiler to treat it as
a GC root, which can cause GC to follow a non-heap pointer → crash.

Fix: `resumeThread(tok)` without annotation.

## Practical Implications

For workloads that make many short C calls from Haskell, batching via Cmm
can reduce per-call overhead by 10-27x. This is most useful when:
- The C calls are cheap (< 100 ns each)
- The calls can be grouped into batches
- The calling thread doesn't need to interact with Haskell between C calls
