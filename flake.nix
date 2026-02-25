{
  description = "GHC Runtime as OpenMP Runtime";

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

        ghcWithCharts = pkgs.haskellPackages.ghcWithPackages (p: [
          p.Chart
          p.Chart-diagrams
        ]);

        mkdocsEnv = pkgs.python3.withPackages (pp: [
          pp.mkdocs
          pp.mkdocs-material
        ]);

        docs = pkgs.stdenv.mkDerivation {
          pname = "ghc-openmp-docs";
          version = "0.18.0";
          src = ./.;
          nativeBuildInputs = [ ghcWithCharts mkdocsEnv pkgs.glibcLocales pkgs.gawk ];
          buildPhase = ''
            export LANG=en_US.UTF-8
            export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive

            # Concatenate section files into single index.md
            cat docs/sections/*.md > docs/index.md

            ghc -O2 -tmpdir /tmp -hidir /tmp -odir /tmp -stubdir /tmp -o gen_charts docs/gen_charts.hs
            (cd docs && ../gen_charts)
            rm gen_charts
            substituteInPlace docs/index.md --replace-quiet GIT_COMMIT "${self.rev or "main"}"

            # Resolve #FN:function_name anchors to line ranges
            for fn in $(grep -oE '#FN:[A-Za-z_0-9]+' docs/index.md | sed 's/#FN://' | sort -u); do
              start=$(grep -n "''${fn}(" src/ghc_omp_runtime_rts.c | head -1 | cut -d: -f1)
              if [ -z "$start" ]; then
                echo "WARNING: no definition for ''${fn}" >&2; continue
              fi
              if sed -n "''${start}p" src/ghc_omp_runtime_rts.c | grep -q '{.*}'; then
                anchor="#L''${start}"
              else
                end=$(awk -v s="$start" 'NR>s && /^\}/ {print NR; exit}' src/ghc_omp_runtime_rts.c)
                anchor="#L''${start}-L''${end}"
              fi
              substituteInPlace docs/index.md --replace-quiet "#FN:''${fn}" "$anchor"
            done

            mkdocs build -d _site
          '';
          installPhase = ''
            cp -r _site $out
          '';
        };
      in
      {
        packages.default = ghc-openmp;
        packages.docs = docs;

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

          docs = flake-utils.lib.mkApp {
            drv = mkRunner "docs" ''
              echo "Serving docs at http://localhost:8080"
              ${pkgs.python3}/bin/python3 -m http.server 8080 -d ${docs}
            '';
          };

          benchmark = flake-utils.lib.mkApp {
            drv = mkRunner "benchmark" ''
              set -e
              p=${ghc-openmp}/bin
              ${pkgs.bash}/bin/bash ${./scripts/run_benchmarks.sh} "$p"
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
