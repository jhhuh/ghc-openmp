## Appendix: GHC RTS Internals

The GHC Runtime System is the execution environment for compiled Haskell
programs. This appendix covers the concepts needed to understand how
our OpenMP runtime integrates with it.

### Capabilities

A **Capability** is GHC's fundamental execution unit. Each Capability
consists of:

- One OS thread (the *owner*)
- One run queue of lightweight Haskell threads (TSOs)
- One spark pool for speculative parallelism (`par`)
- A private allocation area for the generational GC

The number of Capabilities is set by `+RTS -N4` (4 Capabilities).
Each has a 0-indexed number (`cap->no`) that we map directly to
`omp_get_thread_num()`.

### TSOs (Thread State Objects)

A **TSO** represents a lightweight Haskell thread — what `forkIO`
creates. TSOs are much cheaper than OS threads (~1KB vs ~8MB stack).
Thousands of TSOs can be multiplexed onto a single Capability. The
Capability's scheduler picks TSOs from the run queue and executes them
in round-robin fashion, yielding on allocation (every ~4KB allocated).

### Scheduler Loop

Each Capability runs a loop:

```
loop:
  tso = pick from run queue (or steal a spark)
  run tso until it yields/blocks/finishes
  if tso blocked: move to blocked queue
  if tso yielded: put back on run queue
  goto loop
```

When a Capability has no work, it can steal sparks from other
Capabilities or go idle. This is the same work-stealing mechanism
that our OpenMP task implementation builds on.

### RTS API for Embedding

These functions allow C code to interact with the RTS:

| Function | Purpose |
|---|---|
| `hs_init_ghc(&argc, &argv, conf)` | Boot the RTS. Reference-counted: safe to call when already running. |
| `rts_lock()` | Acquire a Capability. Returns `Capability*`. Blocks until one is available. |
| `rts_unlock(cap)` | Release a Capability. Makes it available for Haskell threads or other callers. |
| `rts_setInCallCapability(i, 1)` | Pin the calling OS thread to Capability `i`. Subsequent `rts_lock()` calls will always get Capability `i`. |
| `getNumCapabilities()` | Return the current number of Capabilities. |
| `getNumberOfProcessors()` | Return the CPU count. |

Our runtime uses these to create workers pinned to specific Capabilities.
After the initial `rts_lock()/rts_unlock()` registration, workers release
their Capabilities and become plain OS threads.

### Safe vs Unsafe FFI

GHC provides two FFI calling conventions with different trade-offs:

**Unsafe** (`foreign import ccall unsafe`): The Haskell thread keeps
holding its Capability during the C call. Fast (~2ns overhead), but
blocks all other Haskell threads on that Capability. Suitable for
short, non-blocking C functions.

**Safe** (`foreign import ccall safe`): The Haskell thread releases
its Capability before calling C, and reacquires it on return. Slower
(~68ns overhead) but allows other Haskell threads to run. Required for
C functions that may block or run for a long time.

Internally, safe FFI calls `suspendThread()` (release Capability, return
a token) before the C function, and `resumeThread(token)` (reacquire
Capability) after. This is the mechanism our batched calls exploit
([Section 7.2](#72-batched-safe-calls)).

### Garbage Collection

GHC uses a **stop-the-world** generational garbage collector. When a
GC is triggered:

1. All Capabilities are synchronized (each thread reaches a safe point)
2. GC runs, scanning all Capability-local allocation areas
3. Capabilities are released and threads resume

Critically, GC only synchronizes threads that *hold* Capabilities.
Our OpenMP workers do not hold Capabilities during parallel execution —
they are invisible to the GC. This is why OpenMP compute kernels are
not paused by Haskell garbage collection ([Section 6.4](#64-garbage-collection-isolation)).

### STG Machine Registers

GHC compiles Haskell to STG (Spineless Tagless G-machine) code, which
uses a set of virtual registers mapped to hardware registers:

| Register | x86-64 | Purpose |
|---|---|---|
| BaseReg | `%r13` | Pointer to current Capability |
| Sp | `%rbp` | STG stack pointer |
| Hp | `%r12` | Heap allocation pointer |
| R1 | `%rbx` | First argument / return value |
| R2-R6 | `%r14`, `%rsi`, `%rdi`, `%r8`, `%r9` | Arguments |
| SpLim | `%r15` | Stack limit |

These registers are caller-saved with respect to C calls. Every
`foreign import ccall` must save them before and restore them after the
C function. This is the source of the NCG overhead analyzed in the
[NCG vs LLVM appendix](#appendix-ncg-vs-llvm-code-generation): the NCG
saves/restores these registers inside the loop, while the LLVM backend
hoists them outside.

---

*Environment: NixOS, GHC 9.10.3, GCC 15.2.0, Intel i7-10750H (6C/12T).
Source code: [ghc-openmp](https://github.com/jhhuh/ghc-openmp) repository. February 2026.*
