## Appendix: NCG vs LLVM Code Generation

This appendix details why GHC's default native code generator (NCG) produces
a ~2x slower inner loop than GCC for `sin()`-heavy workloads, and how the
LLVM backend eliminates the gap. See [Section 8.5](#85-ghc-native-parallelism-vs-openmp)
for the benchmark results.

### What the Haskell code compiles to

GHC `-O2` already fully unboxes the inner loop (`sinDouble#`, `+##`, `*##`
on `Double#`), and GCC does not vectorize `sin()` calls. Neither boxing nor
SIMD explains the gap — it is purely a code generator quality issue.

### NCG: 17 instructions per iteration

```asm
cvtsi2sdq %r14, %xmm1        # i -> double
mulsd   .Ln3Sj(%rip), %xmm1  # * 0.001
subq    $8, %rsp              # stack adjust (every iter!)
movsd   %xmm0, %xmm2         # shuffle acc
movsd   %xmm1, %xmm0         # move arg for sin()
movl    $1, %eax              # varargs ABI marker
movsd   %xmm2, 72(%rsp)      # spill acc
movq    %rsi, %rbx            # save STG register
call    sin
addq    $8, %rsp              # restore stack
movsd   64(%rsp), %xmm1      # reload acc
addsd   %xmm0, %xmm1         # acc += sin(...)
incq    %r14                  # i++
movsd   %xmm1, %xmm0         # shuffle back
movq    %rbx, %rsi            # restore STG register
cmpq    %rsi, %r14            # i < hi?
jl      loop
```

The extra instructions come from:

1. **STG register save/restore** (`movq %rsi, %rbx` / `movq %rbx, %rsi`):
   NCG saves R2 around every `sin()` call, inside the loop
2. **Per-iteration stack adjustment** (`subq $8` / `addq $8`): NCG adjusts
   the stack frame every iteration instead of once at function entry
3. **Register shuffles** (`movsd %xmm0, %xmm2`, `movsd %xmm1, %xmm0`,
   `movsd %xmm1, %xmm0`): NCG's linear register allocator produces
   unnecessary moves
4. **Varargs ABI marker** (`movl $1, %eax`): required by x86-64 SysV ABI
   for variadic functions, but GCC elides it when it knows the callee

### LLVM: 10 instructions per iteration

```asm
movsd   %xmm1, 16(%rsp)       # spill acc (same as GCC)
xorps   %xmm0, %xmm0
cvtsi2sd %r14, %xmm0          # i -> double
mulsd   .LCPI20_0(%rip), %xmm0 # * 0.001
callq   sin@PLT
movsd   16(%rsp), %xmm1       # reload acc
addsd   %xmm0, %xmm1          # acc += sin(...)
incq    %r14                   # i++
cmpq    %r14, %r15             # i < hi?
jne     loop
```

LLVM hoists the STG register save/restore outside the loop, allocates the
stack frame once, and eliminates all redundant shuffles. The resulting loop
matches GCC instruction-for-instruction.

### Summary

The 2x NCG gap is not a fundamental Haskell overhead. It is an artifact of
GHC's native code generator producing suboptimal machine code for tight loops
with C calls. The LLVM backend produces identical code quality to GCC,
achieving parity on both sequential and parallel benchmarks.

*Environment: GHC 9.10.3, NCG vs `-fllvm` with LLVM 20.1, GCC 15.2.0.*

---

