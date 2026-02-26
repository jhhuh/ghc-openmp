## Contents

1. [Abstract](#1-abstract)
2. [Motivation](#2-motivation)
3. [Background](#3-background)
4. [Architecture](#4-architecture)
5. [Optimization: From 24x Slower to Parity](#5-optimization-from-24x-slower-to-parity)
6. [Haskell Integration](#6-haskell-integration)
7. [Low-Level Techniques](#7-low-level-techniques)
8. [Shared Memory Demos](#8-shared-memory-demos)
9. [Benchmarks](#9-benchmarks)
10. [Implementation Timeline](#10-implementation-timeline)
11. [Notable Bugs and Fixes](#11-notable-bugs-and-fixes)
12. [Limitations](#12-limitations)
13. [Related Work](#13-related-work)
14. [Conclusions](#14-conclusions)
15. [Appendix: Implemented ABI Surface](#appendix-implemented-abi-surface)
16. [Appendix: GOMP ABI Primer](#appendix-gomp-abi-primer)
17. [Appendix: NCG vs LLVM Code Generation](#appendix-ncg-vs-llvm-code-generation)
18. [Appendix: GHC RTS Internals](#appendix-ghc-rts-internals)
19. [Appendix: Sense-Reversing Barrier](#appendix-sense-reversing-barrier)
20. [Appendix: Zero-Copy FFI](#appendix-zero-copy-ffi-with-pinned-bytearray)
21. [Appendix: Linear Typed Arrays](#appendix-linear-typed-arrays)

---

