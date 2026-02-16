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
      in
      {
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
