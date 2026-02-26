## 13. Related Work

**BOLT** ([bolt-omp.org](https://www.bolt-omp.org/), Best Paper PACT '19) is
the closest analogue to this project. BOLT is a full OpenMP runtime built on
[Argobots](https://www.argobots.org/), a lightweight user-level threading
library from Argonne National Laboratory. Where libgomp maps OpenMP threads
to pthreads, BOLT maps them to Argobots *user-level threads* (ULTs) scheduled
on *execution streams* (ES) — achieving efficient nested parallelism and
fine-grained tasking that pthreads cannot.

The architectural parallel is direct:

| Concept | BOLT / Argobots | ghc-openmp / GHC RTS |
|---------|----------------|---------------------|
| OS-thread abstraction | Execution Stream (ES) | Capability |
| Lightweight work unit | ULT / Tasklet | Haskell green thread |
| OpenMP thread mapping | ULT on ES | OS thread pinned to Capability |
| Scheduler | Pluggable per-pool | GHC spark pool + spin-wait workers |
| Work stealing | Built-in | Phase 15 deferred tasks |

The key difference is motivation: BOLT starts from a *purpose-built* threading
substrate (Argobots) designed for composing HPC runtimes (MPI + OpenMP +
task libraries). ghc-openmp repurposes an *existing language runtime* that
already provides green threads, garbage collection, and an FFI — trading
Argobots' generality for seamless Haskell interoperation.

---

