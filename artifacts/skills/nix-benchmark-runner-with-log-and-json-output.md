# Nix Benchmark Runner with Log Capture and JSON Output

## Problem
Multiple benchmark binaries produce human-readable output. You want:
- `nix run .#benchmark` to run everything
- Raw logs saved for reproducibility
- Parsed results in JSON for programmatic use
- Markdown summary tables for docs/README insertion
- System info captured alongside results

## Architecture

```
scripts/run_benchmarks.sh  ← runner script
  |
  ├── artifacts/bench/system_info.json   ← CPU, kernel, compiler versions
  ├── artifacts/bench/results.json       ← parsed numbers from all benchmarks
  ├── artifacts/bench/summary.md         ← markdown tables ready for docs
  └── artifacts/bench/logs/              ← raw stdout from each benchmark
        ├── bench_overhead_native.log
        ├── bench_overhead_rts.log
        └── ...
```

## Nix app definition
```nix
benchmark = flake-utils.lib.mkApp {
  drv = mkRunner "benchmark" ''
    set -e
    p=${ghc-openmp}/bin
    ${pkgs.bash}/bin/bash ${./scripts/run_benchmarks.sh} "$p"
  '';
};
```

Key: `${./scripts/run_benchmarks.sh}` copies the script into the nix store. The script receives the binary directory as `$1`.

**Important**: The script must be `git add`ed — nix's source filtering only includes tracked files.

## System info collection
```bash
cat > "$OUTDIR/system_info.json" <<EOF
{
  "cpu_model": "$(grep -m1 'model name' /proc/cpuinfo | cut -d: -f2 | xargs)",
  "cpu_cores": $(nproc),
  "kernel": "$(uname -r)",
  "ghc_version": "$(ghc --numeric-version)",
  "gcc_version": "$(gcc -dumpversion)",
  "date": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
```

## Parsing strategy
Each benchmark has fixed printf format strings. Use `grep` + `grep -oE '[0-9]+\.[0-9]+'` to extract floats:

```bash
extract_float() {
    local file="$1" pattern="$2"
    grep "$pattern" "$file" | head -1 | grep -oE '[0-9]+\.[0-9]+' | head -1
}
```

For pipe-delimited table rows (common in Haskell benchmark output):
```bash
grep -E '^\s+[0-9]+\s*\|' "$log" | while IFS='|' read -r col1 col2 col3; do
    val=$(echo "$col2" | grep -oE '[0-9]+\.[0-9]+')
done
```

## Ratio calculation without bc
Use `awk` instead of `bc` (may not be in nix store):
```bash
ratio=$(awk "BEGIN{printf \"%.2f\", $val2 / $val1}")
```

## Writing to CWD
`nix run` executes in the user's CWD, so the script writes directly to `artifacts/bench/`. The results can then be `git add`ed and committed.

## Gotchas
- Subshell pipes (`... | while read`) create subshells — variables set inside don't propagate. Use process substitution or temp files for JSON assembly.
- Some benchmarks may fail (e.g., insufficient threads). Use `|| true` after each run and check log files for content.
- Haskell RTS args need `+RTS -N4` AFTER the binary name, not before.
