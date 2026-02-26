## Low-Level Techniques {#sec:low-level}

This section describes advanced techniques for reducing overhead at the
Haskell-C boundary: zero-overhead Cmm primitives, batched FFI calls,
zero-overhead Cmm primitives and batched FFI calls.

### Cmm Primitives {#sec:cmm-primitives}

*Source: [`omp_prims.cmm`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_prims.cmm), [`HsCmmDemo.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCmmDemo.hs)*

GHC provides three calling conventions for foreign code, each with different
overhead. We wrote a Cmm primitive that reads `Capability_no(MyCapability())`
— the same value as `omp_get_thread_num()` — to measure the overhead of each
tier (see [Section @sec:calling-convention-overhead]).

#### The Cmm Primitive

```c
#include "Cmm.h"

omp_prim_cap_no(W_ dummy) {
    return (Capability_no(MyCapability()));
}
```

Called from Haskell via:

```haskell
foreign import prim "omp_prim_cap_no" primCapNo# :: Int# -> Int#
```

The [`inline-cmm`](https://github.com/jhhuh/inline-cmm) library provides a
`[cmm| ... |]` quasiquoter that eliminates the need for separate `.cmm` files
and manual `foreign import prim` declarations — Template Haskell handles
compilation and linking automatically, producing identical zero-overhead results.

#### Key Findings

**Prim calls are truly free**: GHC treats `foreign import prim` functions as
pure expressions. The Cmm function compiles to a single memory load from
`BaseReg`, which GHC can hoist out of loops entirely via LICM (loop-invariant
code motion). In a tight loop, 100M prim calls complete in <1ms.

**The safe FFI tax is ~65ns**: Each `foreign import ccall safe` call costs ~65ns
more than `unsafe` — the price of `suspendThread()`/`resumeThread()` which
release and reacquire the Capability. For OpenMP regions doing >1us of work,
this is negligible (<7%).

**The callback overhead gap**: The ~500ns per callback overhead from
[Section @sec:bidirectional-callbacks] is ~7x larger than the raw safe FFI
cost (~68ns). The difference comes from `rts_lock()/rts_unlock()` performing
additional work beyond `suspendThread()/resumeThread()`: Task structure
allocation, global Capability search, and lock acquisition. A Cmm-level fast
path could potentially reduce this.

### Batched Safe Calls {#sec:batched-safe-calls}

*Source: [`omp_batch.cmm`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/cbits/omp_batch.cmm), [`HsCmmBatch.hs`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/demos/HsCmmBatch.hs)*

The safe FFI tax of ~68ns per call comes from `suspendThread()`/`resumeThread()`,
which release and reacquire the Capability. For workloads that make many short
C calls, this overhead dominates.

By writing the suspend/resume manually in Cmm, we can batch N C calls within
a single Capability release/reacquire cycle, amortizing the ~68ns overhead
across all N calls.

#### The Batching Primitive

```c
#include "Cmm.h"

cmm_batched_tid(W_ n) {
    W_ tok; W_ result; W_ new_base; W_ stack;
    W_ i; W_ t;

    /* Save Sp to TSO — GC needs valid stack pointer */
    stack = StgTSO_stackobj(CurrentTSO);
    StgStack_sp(stack) = Sp;

    (tok) = ccall suspendThread(BaseReg "ptr", 0);

    result = 0; i = 0;
    goto loop_check;
loop_body:
    (t) = ccall omp_get_thread_num();
    result = result + t;
    i = i + 1;
loop_check:
    if (i < n) goto loop_body;

    (new_base) = ccall resumeThread(tok);
    BaseReg = new_base;

    /* Restore Sp — GC may have moved the stack */
    stack = StgTSO_stackobj(CurrentTSO);
    Sp = StgStack_sp(stack);

    return (result);
}
```

#### Implementation Details

Three details were critical for correctness:

1. **Save/restore Sp**: `suspendThread` releases the Capability, allowing GC to
   run. The GC needs a valid `Sp` in the TSO to scan the suspended thread's
   stack. Without this, the GC follows a stale stack pointer and crashes.

2. **No `"ptr"` on tok**: The token from `suspendThread` is `void*`, not a
   GC-traceable pointer. Annotating it as `"ptr"` would tell GHC's Cmm compiler
   to treat it as a GC root, causing the GC to follow a non-heap pointer.

3. **State# threading**: `foreign import prim` is pure by default — GHC can
   CSE or hoist the call. Threading `State# RealWorld` through the type
   signature makes GHC treat it as effectful while adding zero runtime cost
   (State# is erased at the Cmm level).

Benchmark results showing speedups up to 27x are in
[Section @sec:batched-calls].

---

