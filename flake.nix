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

        # Haskell package via nixpkgs infra (provides Haddock docs)
        ghcOmpHaskellPkg = pkgs.haskellPackages.callCabal2nix "ghc-openmp" ./. {};

        docs = pkgs.stdenv.mkDerivation {
          pname = "ghc-openmp-docs";
          version = "0.18.0";
          src = ./.;
          nativeBuildInputs = [ ghcWithCharts mkdocsEnv pkgs.glibcLocales pkgs.gawk ];
          buildPhase = ''
            export LANG=en_US.UTF-8
            export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive

            # Generate benchmark charts
            ghc -O2 -idocs -tmpdir /tmp -hidir /tmp -odir /tmp -stubdir /tmp -o gen_charts docs/gen_charts.hs
            (cd docs && ../gen_charts)
            rm gen_charts

            # Resolve substitutions on section files (shared by both builds)
            resolve_fn_anchors() {
              local file="$1"
              for fn in $(grep -oE '#FN:[A-Za-z_0-9]+' "$file" | sed 's/#FN://' | sort -u); do
                start=$(grep -n "''${fn}(" cbits/ghc_omp_runtime_rts.c | head -1 | cut -d: -f1)
                if [ -z "$start" ]; then
                  echo "WARNING: no definition for ''${fn}" >&2; continue
                fi
                if sed -n "''${start}p" cbits/ghc_omp_runtime_rts.c | grep -q '{.*}'; then
                  anchor="#L''${start}"
                else
                  end=$(awk -v s="$start" 'NR>s && /^\}/ {print NR; exit}' cbits/ghc_omp_runtime_rts.c)
                  anchor="#L''${start}-L''${end}"
                fi
                substituteInPlace "$file" --replace-quiet "#FN:''${fn}" "$anchor"
              done
            }

            for f in docs/sections/*.md; do
              substituteInPlace "$f" --replace-quiet GIT_COMMIT "${self.rev or "main"}"
              resolve_fn_anchors "$f"
            done

            # === Single-page build ===
            cat docs/sections/*.md > docs/index.md
            mkdocs build -d _site

            # === Multi-page build ===
            mkdir -p docs/pages/charts
            cp docs/charts/*.svg docs/pages/charts/ 2>/dev/null || true

            # Map section files to page names with heading promotion
            promote_headings() {
              sed 's/^##### /#### /; s/^#### /### /; s/^### /## /; s/^## /# /'
            }

            # Frontmatter → index (keep # heading as-is, add single-page link)
            sed 's|Multi-page view](pages/)|Single-page view](../)|' \
              docs/sections/00-frontmatter.md > docs/pages/index.md
            # Skip TOC (01-contents.md) — MkDocs sidebar handles navigation

            promote_headings < docs/sections/02-abstract.md > docs/pages/abstract.md
            promote_headings < docs/sections/03-motivation.md > docs/pages/motivation.md
            promote_headings < docs/sections/04-background.md > docs/pages/background.md
            promote_headings < docs/sections/05-architecture.md > docs/pages/architecture.md
            promote_headings < docs/sections/06-optimization.md > docs/pages/optimization.md
            promote_headings < docs/sections/07-haskell-integration.md > docs/pages/haskell-integration.md
            promote_headings < docs/sections/08-low-level.md > docs/pages/low-level.md
            promote_headings < docs/sections/09-shared-memory.md > docs/pages/shared-memory.md
            promote_headings < docs/sections/10-benchmarks.md > docs/pages/benchmarks.md
            promote_headings < docs/sections/11-timeline.md > docs/pages/timeline.md
            promote_headings < docs/sections/12-bugs.md > docs/pages/bugs.md
            promote_headings < docs/sections/13-limitations.md > docs/pages/limitations.md
            promote_headings < docs/sections/14-related-work.md > docs/pages/related-work.md
            promote_headings < docs/sections/15-conclusions.md > docs/pages/conclusions.md
            promote_headings < docs/sections/A1-abi-surface.md > docs/pages/appendix-abi.md
            promote_headings < docs/sections/A2-gomp-primer.md > docs/pages/appendix-gomp.md
            promote_headings < docs/sections/A3-ncg-llvm.md > docs/pages/appendix-ncg-llvm.md
            promote_headings < docs/sections/A4-rts-internals.md > docs/pages/appendix-rts.md
            promote_headings < docs/sections/A5-barrier.md > docs/pages/appendix-barrier.md
            promote_headings < docs/sections/A6-zero-copy.md > docs/pages/appendix-zero-copy.md
            promote_headings < docs/sections/A7-linear-arrays.md > docs/pages/appendix-linear-arrays.md

            mkdocs build -f mkdocs-multi.yml -d _site/pages

            # Copy Haddock API docs from nixpkgs-built package
            cp -r ${ghcOmpHaskellPkg.doc}/share/doc/ghc-openmp-*/html _site/haddock
          '';
          installPhase = ''
            cp -r _site $out
          '';
        };
      in
      {
        packages.default = ghc-openmp;
        packages.docs = docs;
        packages.haskell = ghcOmpHaskellPkg;

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
