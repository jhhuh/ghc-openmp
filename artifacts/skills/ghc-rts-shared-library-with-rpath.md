# Building a Self-Contained Shared Library with GHC RTS

## Problem
You want to build `libfoo.so` from C code that uses GHC RTS APIs (`Rts.h`, `hs_init`, etc.). C consumers should link with `-lfoo` without knowing about GHC.

## Key Insight
GHC's shared libraries (`libHSbase`, `libHSghc-prim`, etc.) do NOT list `libHSrts_thr.so` as a dependency. When GHC links a shared library with `-shared -dynamic`, the RTS is NOT included transitively. You must explicitly link it and embed rpath.

## Build Steps

### 1. Discover RTS paths
```makefile
GHC_LIBDIR := $(shell $(GHC) --print-libdir)
GHC_VER    := $(shell $(GHC) --numeric-version)
RTS_SODIR  := $(shell find $(GHC_LIBDIR) -maxdepth 2 \
                -name 'libHSrts*_thr-ghc*.so' -print -quit | xargs dirname)
RTS_LIBNAME := $(shell find $(GHC_LIBDIR) -maxdepth 2 \
                -name 'libHSrts*_thr-ghc*.so' -print -quit | \
                xargs basename | sed 's/^lib//; s/\.so$$//')
```

### 2. Compile with -fPIC
```makefile
build/runtime_pic.o: src/runtime.c
    $(CC) -DTHREADED_RTS -I$(RTS_INCDIR) -fPIC -O2 -c $< -o $@

build/HsStub_pic.o: src/HsStub.hs
    $(GHC) -threaded -dynamic -c $< -o $@
```

### 3. Link with GHC, explicitly adding RTS + rpath
```makefile
build/libfoo.so: build/runtime_pic.o build/HsStub_pic.o
    $(GHC) -shared -dynamic -threaded -no-hs-main \
        build/runtime_pic.o build/HsStub_pic.o \
        -o $@ -lpthread -lm \
        -optl-L$(RTS_SODIR) -optl-l$(RTS_LIBNAME) \
        -optl-Wl,-rpath,$(RTS_SODIR) \
        -optl-Wl,-soname,libfoo.so
```

### 4. Verify
```bash
ldd build/libfoo.so | grep rts  # should show libHSrts_thr
gcc -fopenmp test.c -Lbuild -Wl,-rpath,$PWD/build -lfoo -o test && ./test
```

## Why HsStub.hs is needed
A minimal `module HsStub where` pulls in GHC's `base` library closures. Without it, `hs_init_ghc()` can't find the base package.

## pkg-config integration
Create `foo.pc.in` with just `-lfoo` in Libs (no GHC flags needed — rpath handles everything):
```
Libs: -L${libdir} -lfoo
```

## Gotchas
- The RTS `.so` path is version-specific (e.g., `libHSrts-1.0.2_thr-ghc9.10.3.so`). The `find` discovery handles this.
- In nix, `GHC_LIBDIR` is a nix store path. The rpath embeds this, so the .so works as long as the nix store path exists.
- For cabal packages, do NOT link the .so — use `c-sources` instead so the consumer's own GHC compiles the C code directly. This avoids RTS ABI mismatches.
