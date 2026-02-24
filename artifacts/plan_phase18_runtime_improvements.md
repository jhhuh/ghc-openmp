# Phase 18: Five Runtime Improvements

## Steps
1. True guided scheduling (CAS loop with exponentially-decreasing chunks)
2. Hybrid spin-sleep barriers (OMP_WAIT_POLICY support)
3. Pre-allocated task descriptor pool (eliminate malloc/free per task)
4. Per-Capability task queues with work stealing (replace global mutex queue)
5. Serialized nested parallelism with level tracking

## Target file
`src/ghc_omp_runtime_rts.c`
