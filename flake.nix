{
  description = "GHC Runtime as OpenMP Runtime - exploration project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        ghc-openmp = pkgs.stdenv.mkDerivation {
          pname = "ghc-openmp";
          version = "0.18.0";
          src = self;

          nativeBuildInputs = with pkgs; [
            ghc
            gnumake
            findutils   # for find in Makefile RTS_INCDIR discovery
          ];

          # stdenv provides gcc with -fopenmp support
          buildPhase = ''
            make build-all
          '';

          installPhase = ''
            make install PREFIX=$out DESTDIR=
          '';
        };

        # Helper to make a runner script
        mkRunner = name: script: pkgs.writeShellScriptBin name script;
      in
      {
        packages.default = ghc-openmp;

        apps = {
          test-all = flake-utils.lib.mkApp {
            drv = mkRunner "test-all" ''
              set -e
              p=${ghc-openmp}/bin
              echo "=== Running with native libgomp ==="
              OMP_NUM_THREADS=4 $p/test_omp_basic_native
              echo ""
              echo "=== Running with GHC-OMP runtime ==="
              OMP_NUM_THREADS=4 $p/test_omp_basic_ghcomp
              echo ""
              echo "=== Running GHC RTS embedding test ==="
              $p/test_rts_embed
              echo ""
              echo "=== Running with GHC RTS-backed runtime ==="
              OMP_NUM_THREADS=4 $p/test_omp_rts
              echo ""
              echo "=== Haskell-OpenMP Interop Demo ==="
              $p/hs_omp_demo +RTS -N4
              echo ""
              echo "=== Guided: Native libgomp ==="
              OMP_NUM_THREADS=4 $p/test_guided_native
              echo ""
              echo "=== Guided: GHC RTS-backed ==="
              OMP_NUM_THREADS=4 $p/test_guided_rts
              echo ""
              echo "=== Nested: Native libgomp ==="
              OMP_NUM_THREADS=4 $p/test_nested_native
              echo ""
              echo "=== Nested: GHC RTS-backed ==="
              OMP_NUM_THREADS=4 $p/test_nested_rts
            '';
          };

          bench = flake-utils.lib.mkApp {
            drv = mkRunner "bench" ''
              p=${ghc-openmp}/bin
              echo "=== Native libgomp ==="
              OMP_NUM_THREADS=4 $p/bench_native
              echo ""
              echo "=== GHC RTS-backed ==="
              OMP_NUM_THREADS=4 $p/bench_rts
            '';
          };

          test-tasks = flake-utils.lib.mkApp {
            drv = mkRunner "test-tasks" ''
              p=${ghc-openmp}/bin
              echo "=== Tasks: Native libgomp ==="
              OMP_NUM_THREADS=4 $p/test_tasks_native
              echo ""
              echo "=== Tasks: GHC RTS-backed ==="
              OMP_NUM_THREADS=4 $p/test_tasks_rts
            '';
          };

          bench-dgemm = flake-utils.lib.mkApp {
            drv = mkRunner "bench-dgemm" ''
              p=${ghc-openmp}/bin
              for t in 1 2 4 8; do
                echo "=== $t threads: Native libgomp ==="
                OMP_NUM_THREADS=$t $p/bench_dgemm_native
                echo ""
                echo "=== $t threads: GHC RTS-backed ==="
                OMP_NUM_THREADS=$t $p/bench_dgemm_rts
                echo ""
              done
            '';
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # GHC and Haskell tooling
            ghc
            cabal-install
            haskell-language-server

            # C toolchain
            gcc
            clang
            gnumake
            cmake

            # OpenMP references
            llvmPackages.openmp  # libomp for reference

            # Debugging / analysis
            gdb
            valgrind
            linuxPackages.perf

            # Documentation
            man-pages
            man-pages-posix
          ];

          shellHook = ''
            echo "ghc-openmp dev environment loaded"
            echo "GHC: $(ghc --version)"
            echo "GCC: $(gcc --version | head -1)"
          '';
        };
      });
}
