# GHC RTS Internals — OpenMP Feasibility Analysis

## Source: GHC 9.10.3, `rts/` directory

---

## 1. Key Data Structures

### Capability (`rts/Capability.h`)
The central execution unit. **One Capability per OS thread.** This is the closest
analog to an OpenMP thread.

```c
struct Capability_ {
    StgFunTable f;
    StgRegTable r;
    uint32_t no;              // capability number (0-indexed!)  ← OpenMP thread ID
    uint32_t node;            // NUMA node
    Task *running_task;       // OS thread owning this Capability
    bool in_haskell;          // currently running Haskell code?
    bool disabled;
    // Per-capability run queue
    StgTSO *run_queue_hd;
    StgTSO *run_queue_tl;
    uint32_t n_run_queue;
    // Work-stealing spark pool (THREADED_RTS only)
    SparkPool *sparks;        // WSDeque of StgClosure*
    SparkCounters spark_stats;
    // Synchronization
    Mutex lock;               // protects shared fields
    Task *returning_tasks_hd; // tasks waiting to return from FFI
    Message *inbox;           // inter-capability messages
    // GC, STM, I/O manager fields...
};
```

**Key insight**: `cap->no` is a 0-indexed integer, exactly what `omp_get_thread_num()` returns.

### Global state
```c
extern uint32_t n_capabilities;       // current count
extern uint32_t enabled_capabilities; // non-disabled count
extern uint32_t max_n_capabilities;   // max (for array sizing)
extern Capability **capabilities;     // array of all Capabilities
```

### Task (`rts/Task.h`)
Represents an OS thread bound to the RTS. A Task acquires a Capability to execute.

### TSO (Thread State Object)
A lightweight Haskell thread. Many TSOs can be multiplexed onto one Capability.

### SparkPool = WSDeque (`rts/WSDeque.h`)
Chase-Lev work-stealing deque. Per-capability, lock-free.

```c
typedef struct WSDeque_ {
    StgInt size;
    StgWord moduloSize;  // bitmask
    StgInt top;          // steal end (CAS-protected)
    StgInt bottom;       // push end (owner-only)
    void **elements;
} WSDeque;
```

---

## 2. Public C APIs (callable from external C code)

### RTS Lifecycle (`rts/include/RtsAPI.h`)
```c
void hs_init_ghc(int *argc, char **argv[], RtsConfig rts_config);
void hs_exit(void);
```

### Capability Locking
```c
Capability *rts_lock(void);           // acquire a capability
void rts_unlock(Capability *token);   // release
Capability *rts_unsafeGetMyCapability(void); // get current (unsafe FFI only)
void rts_setInCallCapability(int cap, int affinity); // pin to specific cap
void rts_pinThreadToNumaNode(int node);
```

### Pause/Resume (global barrier-like)
```c
PauseToken *rts_pause(void);    // acquires ALL capabilities, stops all Haskell threads
void rts_resume(PauseToken *);  // releases all
bool rts_isPaused(void);
```

### Thread Management (`rts/include/rts/Threads.h`)
```c
StgTSO *createThread(Capability *cap, W_ stack_size);
StgTSO *createIOThread(Capability *cap, W_ stack_size, StgClosure *closure);
void scheduleWaitThread(StgTSO *tso, HaskellObj *ret, Capability **cap);
uint32_t getNumCapabilities(void);
void setNumCapabilities(uint32_t new_);  // NOTE: only supports INCREASING
```

### Parallelism (`rts/include/rts/Parallel.h`)
```c
StgInt newSpark(StgRegTable *reg, StgClosure *p);  // create a spark
```

### OS Threads (`rts/include/rts/OSThreads.h`)
```c
int createOSThread(OSThreadId *tid, const char *name, OSThreadProc *proc, void *param);
void initMutex(Mutex *pMut);
void closeMutex(Mutex *pMut);
void initCondition(Condition *pCond);
void broadcastCondition(Condition *pCond);
void signalCondition(Condition *pCond);
void waitCondition(Condition *pCond, Mutex *pMut);
bool timedWaitCondition(Condition *pCond, Mutex *pMut, Time timeout);
void newThreadLocalKey(ThreadLocalKey *key);
void *getThreadLocalVar(ThreadLocalKey *key);
void setThreadLocalVar(ThreadLocalKey *key, void *value);
void setThreadAffinity(uint32_t n, uint32_t m);
uint32_t getNumberOfProcessors(void);
```

### Atomics (`rts/include/stg/SMP.h`)
```c
StgWord xchg(StgPtr p, StgWord w);           // atomic exchange
StgWord cas(StgVolatilePtr p, StgWord o, StgWord n);  // CAS
StgWord atomic_inc(StgVolatilePtr p, StgWord n);
StgWord atomic_dec(StgVolatilePtr p, StgWord n);
// Memory ordering macros: RELAXED_LOAD, ACQUIRE_LOAD, RELEASE_STORE, etc.
```

### Spin Locks (`rts/include/rts/SpinLock.h`)
```c
void initSpinLock(SpinLock *p);
ACQUIRE_SPIN_LOCK(p);
RELEASE_SPIN_LOCK(p);
```

---

## 3. Internal APIs (would need to link against RTS internals)

### Capability Management (`rts/Capability.h`)
```c
void initCapabilities(void);
void moreCapabilities(uint32_t from, uint32_t to);
Capability *getCapability(uint32_t i);
void releaseCapability(Capability *cap);
void waitForCapability(Capability **cap, Task *task);
bool yieldCapability(Capability **pCap, Task *task, bool gcAllowed);
bool tryGrabCapability(Capability *cap, Task *task);
void stopAllCapabilities(Capability **pCap, Task *task); // global sync
void prodAllCapabilities(void);
void contextSwitchAllCapabilities(void);
StgClosure *findSpark(Capability *cap);  // steal from other caps
bool anySparks(void);
```

### Spark Pool (`rts/Sparks.h`)
```c
SparkPool *allocSparkPool(void);
StgClosure *reclaimSpark(SparkPool *pool);       // owner pop
StgClosure *tryStealSpark(SparkPool *pool);       // steal from read end
void createSparkThread(Capability *cap);           // convert spark → thread
bool fizzledSpark(StgClosure *spark);
```

### WSDeque (`rts/WSDeque.h`)
```c
WSDeque *newWSDeque(uint32_t size);
bool pushWSDeque(WSDeque *q, void *elem);  // owner push
void *popWSDeque(WSDeque *q);               // owner pop
void *stealWSDeque_(WSDeque *q);            // steal (no retry)
void *stealWSDeque(WSDeque *q);             // steal (with retry)
```

### Scheduler (`rts/Schedule.c`)
```c
void initScheduler(void);
static void scheduleActivateSpark(Capability *cap);  // spark → thread
static void acquireAllCapabilities(Capability *cap, Task *task);
static void startWorkerTasks(uint32_t from, uint32_t to);
```

---

## 4. OpenMP ↔ GHC RTS Mapping

| OpenMP Concept | GHC RTS Primitive | Notes |
|---|---|---|
| `omp_get_num_threads()` | `getNumCapabilities()` | **Direct** |
| `omp_get_thread_num()` | `cap->no` via `rts_unsafeGetMyCapability()` | **Direct** |
| `omp_set_num_threads(N)` | `setNumCapabilities(N)` | Only increases! |
| `omp_get_num_procs()` | `getNumberOfProcessors()` | **Direct** |
| Thread team (fork) | Capabilities are persistent; dispatch work to all | Conceptual gap |
| Thread team (join/barrier) | `rts_pause()`/`rts_resume()` or custom barrier | `rts_pause` is too heavy |
| `omp parallel` body | Enqueue work on each cap's run queue or spark pool | Possible |
| `omp for` | Partition by `cap->no` / `n_capabilities` | Manual partitioning |
| `omp task` | `newSpark()` or `createIOThread()` | Sparks = lightweight tasks |
| `omp critical` | `Mutex` from OSThreads | **Direct** |
| `omp atomic` | `cas()`, `atomic_inc()`, `atomic_dec()` | **Direct** |
| `omp barrier` | Custom: atomic counter + `Condition` | Must build |
| `omp single` | CAS on per-region flag | Must build |
| `omp reduction` | Per-capability accumulator + merge | Must build |
| `threadprivate` | `ThreadLocalKey` (OS TLS) | **Direct** (caps are on OS threads) |
| Nested parallelism | Not feasible (setNumCapabilities only grows) | **Limitation** |
| `omp ordered` | Condition wait on thread-ID sequence | Must build |

---

## 5. Key Architectural Observations

### What works well
1. **Capability number = thread ID**: `cap->no` is exactly `omp_get_thread_num()`
2. **Fixed thread pool**: GHC already maintains a pool of N OS threads (one per capability), similar to OpenMP's thread pool
3. **Work stealing exists**: The spark pool deque is a production-quality work-stealing implementation
4. **OS-level primitives available**: Mutexes, conditions, TLS, atomics — all exposed
5. **Embedding API exists**: `rts_lock/unlock`, `hs_init_ghc` provide a clean C embedding story

### What requires work
1. **No fork-join**: GHC doesn't have "fork N threads, run closure, join" — we need to build this on top of the per-capability run queues or spark pools
2. **Barrier**: No built-in barrier. `rts_pause()` pauses everything including the scheduler — too heavy. Need a custom sense-reversing barrier using atomics+conditions.
3. **setNumCapabilities only increases**: Can't shrink thread team for nested parallelism
4. **Spark pool is for thunks**: Sparks are `StgClosure*` — evaluated Haskell thunks, not arbitrary C function pointers. For C OpenMP work, we'd need to wrap C functions in closures or use a different dispatch mechanism.
5. **GC pauses**: Stop-the-world GC will pause all OpenMP threads. The nonmoving GC helps but doesn't eliminate pause-time.

### Critical design question
Can we dispatch arbitrary C function pointers to Capabilities without going through Haskell closures?
- **Option A**: Use `createOSThread` to create plain OS threads that acquire capabilities via `rts_lock()` → this gives us capability-numbered threads, but doesn't use the spark pool
- **Option B**: Wrap C function pointers in `StgClosure` (via `rts_mkFunPtr` + `rts_apply`) and use sparks → uses work stealing, but adds overhead
- **Option C**: Add a new "C task queue" alongside the spark pool in each Capability → most performant, but requires RTS modification
