# Cabal Package with C Sources and Data Files

## Problem
You have a C library that uses GHC RTS APIs. You want:
- Haskell consumers to `build-depends: your-pkg` and get the C code compiled with their own GHC
- C consumers to get a shared library (.so) and pkg-config file
- No RTS ABI conflicts between the package's GHC and the consumer's GHC

## Solution: c-sources for Haskell, data-dir for C artifacts

### Cabal file
```cabal
cabal-version: 2.4
name:          your-pkg
version:       0.1.0
data-dir:      data
data-files:    your-pkg.pc.in

library
  exposed-modules:  Your.Module
  other-modules:    Paths_your_pkg
  autogen-modules:  Paths_your_pkg
  hs-source-dirs:   lib
  c-sources:        src/your_runtime.c
  cc-options:       -DTHREADED_RTS -O2
  ghc-options:      -threaded
  build-depends:    base >= 4.14 && < 5
  default-language: Haskell2010
```

### Key design decisions

1. **`c-sources` in `library`**: The C file is compiled by the consumer's GHC with their RTS headers. No ABI mismatch possible.

2. **No `foreign-library` stanza**: A `foreign-library` would build a .so that links against a specific GHC RTS version. If the consumer has a different GHC, symbols conflict.

3. **`data-files` for pkg-config template**: Ship the `.pc.in` template so C consumers can find it via `Paths_your_pkg.getDataFileName`.

4. **`.so` built separately**: The shared library is platform-specific and must be built with `make` or `nix build`, not shipped in `cabal sdist`.

### Haskell module
```haskell
module Your.Module where
import Paths_your_pkg (getDataFileName)

foreign import ccall unsafe "your_func" yourFunc :: IO Int

pkgConfigTemplatePath :: IO FilePath
pkgConfigTemplatePath = getDataFileName "your-pkg.pc.in"
```

### FFI calling conventions
- Use `ccall unsafe` for functions that don't call back into Haskell and are fast (< microsecond). This avoids the ~68ns safe FFI overhead.
- Use `ccall safe` for functions that may call back into Haskell or take a long time, since `safe` releases the Capability for other Haskell threads.

## Multiple .cabal files in one repo
Cabal does NOT support multiple `.cabal` files in the same directory. If you have a demo/test package:
1. Move it to a subdirectory: `demos/your-demo/your-demo.cabal`
2. Update `cabal.project`:
   ```
   packages:
     .
     demos/your-demo
   ```
3. Fix `hs-source-dirs` in the moved cabal file to use relative paths (e.g., `../../src`).

## Gotchas
- GHC automatically provides `Rts.h` include paths for `c-sources` — no `include-dirs` needed
- `cc-options: -DTHREADED_RTS` is required if the C source includes `Rts.h` and uses threaded RTS APIs
- `ghc-options: -threaded` ensures the threaded RTS is linked
- `autogen-modules: Paths_your_pkg` is required in cabal 2.4+ when listing `Paths_*` in `other-modules`
