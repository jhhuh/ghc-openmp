# Phase 11: inline-cmm Integration

## Setup

- inline-cmm library from github.com/jhhuh/inline-cmm (git dependency)
- Built via cabal with cabal.project source-repository-package
- GHC 9.10.3, -threaded -O2, -N4

## Results

All inline Cmm primitives work correctly:

| Primitive | Input | Output | Status |
|---|---|---|---|
| `get_cap_no` | dummy | 0 (Capability 0) | OK |
| `cmm_add` | 3, 5 | 8 | OK |
| `cmm_mul` | 4, 7 | 28 | OK |
| `cmm_fma` | 2, 3, 10 | 16 (2*3+10) | OK |

Benchmark: `get_cap_no` via inline-cmm quasiquoter achieves same ~0 ns/call
as the hand-written Cmm from Phase 10.

## What inline-cmm Automates

Given this Haskell code:

```haskell
include "\"Cmm.h\""

[cmm|
I64 get_cap_no(I64 dummy) {
    return (Capability_no(MyCapability()));
}
|]
```

The quasiquoter:
1. Parses the Cmm function signature (`I64 get_cap_no(I64 dummy)`)
2. Generates `foreign import prim "get_cap_no" get_cap_no# :: Int# -> Int#`
3. At TH finalization, writes the Cmm to a temp file
4. Compiles it via `ghc -c -x cmm`
5. Links the object file into the binary via `addForeignFilePath`

## Comparison: Hand-written vs inline-cmm

### Hand-written (Phase 10)

Requires:
- Separate `omp_prims.cmm` file
- Manual `foreign import prim` declaration in Haskell
- Makefile target: `ghc -c -x cmm src/omp_prims.cmm -o build/omp_prims.o`
- Linking: include `build/omp_prims.o` in ghc invocation

### inline-cmm (Phase 11)

Requires:
- `inline-cmm` as a cabal dependency
- `[cmm| ... |]` quasiquoter in Haskell source
- Nothing else — TH handles compilation and linking

## Files

- `src/HsInlineCmm.hs` — Demo using quasiquoter
- `ghc-openmp.cabal` — Cabal package with inline-cmm dependency
- `cabal.project` — Source repository package for inline-cmm
