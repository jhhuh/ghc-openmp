## 13. Conclusions

GHC's Runtime System can serve as a fully functional OpenMP runtime with
**zero measurable overhead** compared to native libgomp. The
implementation is a single ~1300-line C file using only public GHC RTS APIs —
no GHC fork required.

The key architectural insights are:

1. **Capabilities as thread IDs**: `cap->no`
   directly maps to `omp_get_thread_num()`
2. **Workers without Capabilities**: After RTS registration,
   worker threads release their Capabilities. They execute C code as plain OS
   threads, invisible to GC.
3. **Reference-counted init**: `hs_init_ghc()`
   is idempotent, enabling transparent use from both C and Haskell hosts.
4. **Lock-free synchronization is essential**: The naive
   mutex+condvar implementation was 20–25x slower. Sense-reversing barriers
   and atomic generation counters brought it to parity.
5. **Bidirectional FFI works**: OpenMP workers call Haskell
   functions via `FunPtr` with ~0.5us overhead per invocation
   (automatic `rts_lock/unlock`), making it practical for
   coarse-grained callbacks.

This demonstrates that language runtimes can share threading infrastructure
across FFI boundaries. A Haskell program can call OpenMP C code, with both
sharing the same thread pool, the same CPU cores, and coexisting with GHC's
garbage collector.

Beyond performance parity, unifying the runtimes enables a new programming
model: Haskell and C code operating on the same data with type-safe
guarantees. Linear tokens prove disjoint access at compile time,
eliminating defensive synchronization. The type checker becomes a
concurrency tool — data races are compile errors, not runtime surprises.

---

