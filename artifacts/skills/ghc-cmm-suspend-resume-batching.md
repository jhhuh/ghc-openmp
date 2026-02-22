# Batched safe FFI calls via Cmm suspend/resume

## Problem

`foreign import ccall safe` costs ~65ns per call (suspendThread/resumeThread).

## Solution

Cmm calls suspendThread once, makes N C calls, calls resumeThread once.
At batch=100, per-call cost drops to 2.7ns.

## Critical details

1. **Save Sp before suspendThread** — GC needs valid stack pointer to scan
   suspended thread. Without it → segfault.
2. **No "ptr" on resumeThread token** — it's opaque void*, not GC-traceable.
3. **Thread State# RealWorld** — prevents GHC from CSE/LICM of side effects.
4. **Reassign BaseReg** — resumeThread may return different Capability.
