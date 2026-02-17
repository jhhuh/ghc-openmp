# Mitigating thermal variance in laptop benchmarks

## Problem

Laptop CPUs with powersave governor produce variable results. Single-sample
measurements can show 2x differences due to CPU frequency scaling and turbo
boost decay. This caused a false 1.65x "regression" in ghc-openmp Phase 3.

## Mitigations

1. **Best-of-N within a single process** (N=5-10, report minimum)
2. **Interleaved comparison** (alternate native/RTS, same thermal conditions)
3. **Warmup iteration** (throw away first run, CPU needs ~100ms to ramp)
4. **Performance governor** (`sudo cpupower frequency-set -g performance`)

## Key insight

If two implementations show different performance but the ratio fluctuates
between runs, the difference is thermal variance, not a real regression.
Overlapping distributions = no regression.
