# Hakyll in a Nix Flake (single-page site)

## Pattern

Build a Hakyll static site inside a nix flake derivation without a separate
cabal project. Compile `site.hs` directly with `ghcWithPackages`.

## Nix derivation

```nix
ghcWithHakyll = pkgs.haskellPackages.ghcWithPackages (p: [ p.hakyll ]);

docs = pkgs.stdenv.mkDerivation {
  pname = "my-docs";
  version = "0.1.0";
  src = ./docs;
  nativeBuildInputs = [ ghcWithHakyll pkgs.glibcLocales ];
  buildPhase = ''
    export LANG=en_US.UTF-8
    export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
    ghc -O2 -threaded -o site site.hs
    ./site build
  '';
  installPhase = ''
    mkdir -p $out
    cp -r _site/* $out/
  '';
};
```

## Critical: UTF-8 locale

Hakyll fails with "cannot decode byte sequence starting from 226" without
`glibcLocales` + `LANG` + `LOCALE_ARCHIVE`.

## Heading ID mismatch (pandoc vs Jekyll)

Pandoc strips leading numbers from heading IDs. Fix with
`pandocCompilerWithTransform` and custom AST walker.

## GitHub Actions deployment

Switch Pages from Jekyll to Actions:
```bash
gh api repos/OWNER/REPO/pages -X PUT -f build_type=workflow
```
