## 3. Background

### 3.1 GHC RTS Capabilities

A *Capability* is GHC's central execution unit: one OS thread, one
run queue of lightweight Haskell threads (TSOs), and one work-stealing spark
pool. The number of Capabilities is set by `+RTS -N`. Each
Capability has a 0-indexed number (`cap->no`) that maps directly to
OpenMP's `omp_get_thread_num()`.

Key RTS APIs for embedding:

```c
hs_init_ghc(&argc, &argv, conf);       // Boot the RTS
Capability *cap = rts_lock();           // Acquire a Capability
rts_unlock(cap);                        // Release it
rts_setInCallCapability(i, 1);          // Pin OS thread to Capability i
uint32_t getNumCapabilities(void);      // Current Capability count
uint32_t getNumberOfProcessors(void);   // CPU count
```

`hs_init_ghc()` is reference-counted: calling it when the RTS is
already running (as in a Haskell host program) simply increments the counter
and returns. This is the key to transparent interop — our runtime
auto-detects whether it is being hosted by a C program or a Haskell program.

### 3.2 The libgomp ABI

*Source: [`ghc_omp_runtime_rts.c`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/src/ghc_omp_runtime_rts.c)*

GCC transforms OpenMP pragmas into calls to `GOMP_*` functions.
For example:

```c
#pragma omp parallel
{ body; }

// becomes:
void outlined_fn(void *data) { body; }
GOMP_parallel(outlined_fn, &data, num_threads, flags);
```

A minimum viable runtime needs only 9 symbols (`GOMP_parallel`,
`GOMP_barrier`, `GOMP_critical_start/end`,
`GOMP_single_start`, `GOMP_task`,
`GOMP_taskwait`, `omp_get_num_threads`,
`omp_get_thread_num`). Full OpenMP 4.5 coverage requires ~85
symbols. Our implementation provides ~97.

### 3.3 Cmm and `foreign import prim`

*Source: [`omp_prims.cmm`](https://github.com/jhhuh/ghc-openmp/blob/GIT_COMMIT/src/omp_prims.cmm)*

[Cmm](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/cmm-type)
(C minus minus) is GHC's low-level intermediate representation — a portable
assembly language that sits between STG and native code. GHC compiles all
Haskell to Cmm before generating machine code.

GHC provides three FFI calling conventions with different overhead:

| Convention | Mechanism | Overhead |
|---|---|---|
| `foreign import ccall safe` | Releases Capability, calls C, reacquires | ~68 ns |
| `foreign import ccall unsafe` | Saves STG registers, calls C, restores | ~2 ns |
| `foreign import prim` | Direct STG register passing, no boundary | ~0 ns |

The `prim` convention is the fastest: arguments pass directly in GHC's STG
registers (R1, R2, ...) with no calling convention switch. Functions written
in Cmm can access RTS internals like `MyCapability()` directly. GHC treats
`prim` calls as pure expressions and can optimize them away entirely
(loop-invariant code motion, common subexpression elimination).

The [`inline-cmm`](https://github.com/jhhuh/inline-cmm) library lets you
embed Cmm code directly in Haskell modules via a `[cmm| ... |]` quasiquoter
(similar to `inline-c` for C code). It automatically generates the
`foreign import prim` declaration and compiles the Cmm via Template Haskell.

---

