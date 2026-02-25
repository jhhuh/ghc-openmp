# Build-Time Source Code Line Range Linker

## Problem

Documentation links to source code on GitHub using `#L<number>` anchors break when the source file is edited (lines shift). Hardcoding line numbers requires manual updates every commit.

## Solution

Use `#FN:function_name` placeholders in markdown. At build time, a shell script greps the source file to resolve each placeholder to `#L<start>-L<end>` (full function body range) or `#L<start>` (one-liners).

## Markdown Pattern

```markdown
[`GOMP_parallel`](https://github.com/user/repo/blob/GIT_COMMIT/src/file.c#FN:GOMP_parallel)
```

The `GIT_COMMIT` placeholder is separately resolved to the actual commit hash via `substituteInPlace`.

## Nix Build Step

Add to `buildPhase` in a nix derivation, after `GIT_COMMIT` substitution:

```nix
buildPhase = ''
  # ... other build steps ...

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

  # ... mkdocs build ...
'';
```

Note: `''${fn}` is nix escaping for `${fn}` inside `'' ''` strings. Add `pkgs.gawk` to `nativeBuildInputs` for the `awk` command.

## How It Works

1. **Find start**: `grep -n "funcname(" file.c | head -1` — matches function definition (name followed by `(`), takes first match
2. **One-liner check**: `sed -n "${start}p" | grep -q '{.*}'` — if both `{` and `}` on same line, it's a one-liner
3. **Find end**: `awk 'NR>s && /^\}/ {print NR; exit}'` — scans forward for closing `}` at column 0

## Assumptions

- C functions defined at column 0 (not indented)
- Closing `}` at column 0 for multi-line functions
- Function name followed directly by `(` in the definition
- No forward declarations that would confuse `head -1`

## Result on GitHub

- One-liner functions: `#L588` highlights single line
- Multi-line functions: `#L412-L525` highlights the full function body with yellow background

## Files

- `docs/index.md` — contains `#FN:` placeholders
- `flake.nix` — resolution script in `docs` derivation `buildPhase`
