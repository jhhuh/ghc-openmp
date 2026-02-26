#!/usr/bin/env bash
# Update benchmark tables in README.md and generate chart data module
# from artifacts/bench/ data (logs + results.json).
#
# README tables are delimited by <!-- BENCH:key --> / <!-- /BENCH:key --> markers.
# Docs section tables are injected at build time by scripts/preprocess_docs.py.
#
# Usage: scripts/update_bench_tables.sh [bench_dir]
#   bench_dir defaults to artifacts/bench

set -euo pipefail

BENCH="${1:-artifacts/bench}"
LOGDIR="$BENCH/logs"
JSON="$BENCH/results.json"
README="README.md"

die() { echo "ERROR: $*" >&2; exit 1; }
[ -f "$JSON" ] || die "$JSON not found. Run benchmarks first: nix run .#benchmark"

# ── helpers ──────────────────────────────────────────────────────────

# Replace content between <!-- BENCH:$key --> and <!-- /BENCH:$key -->
# Args: file key content_string
replace_section() {
    local file="$1" key="$2" content="$3"
    local start_marker="<!-- BENCH:$key -->"
    local end_marker="<!-- /BENCH:$key -->"
    grep -q "$start_marker" "$file" || { echo "  SKIP: no $start_marker in $file"; return; }
    awk -v sm="$start_marker" -v em="$end_marker" -v content="$content" '
        $0 == sm  { print; printf "%s\n", content; skip=1; next }
        $0 == em  { skip=0 }
        !skip     { print }
    ' "$file" > "$file.tmp" && mv "$file.tmp" "$file"
    echo "  Updated $key in $file"
}

# Extract a float after a pattern from a log file
extract_float() {
    local file="$1" pattern="$2"
    grep "$pattern" "$file" | head -1 | grep -oE '[0-9]+\.[0-9]+' | head -1
}

# Format ratio with annotation (e.g. "0.86x (faster)" or "1.12x")
format_ratio() {
    local ratio="$1"
    if awk "BEGIN{exit !($ratio < 0.95)}" 2>/dev/null; then
        printf "**%sx** (RTS faster)" "$ratio"
    elif awk "BEGIN{exit !($ratio > 1.05)}" 2>/dev/null; then
        printf "%sx" "$ratio"
    else
        printf "%sx (parity)" "$ratio"
    fi
}

# ── README: microbenchmarks (4-thread summary) ──────────────────────

gen_readme_micro() {
    local n_fj r_fj n_bar r_bar n_pf r_pf n_cr r_cr
    local log_n="$LOGDIR/bench_overhead_native_4t.log"
    local log_r="$LOGDIR/bench_overhead_rts_4t.log"
    # Fall back to non-suffixed logs if multi-thread logs don't exist
    [ -f "$log_n" ] || log_n="$LOGDIR/bench_overhead_native.log"
    [ -f "$log_r" ] || log_r="$LOGDIR/bench_overhead_rts.log"
    [ -f "$log_n" ] || return

    n_fj=$(extract_float "$log_n" "Fork/join")
    r_fj=$(extract_float "$log_r" "Fork/join")
    n_bar=$(extract_float "$log_n" "Barrier")
    r_bar=$(extract_float "$log_r" "Barrier")
    n_pf=$(extract_float "$log_n" "Parallel for")
    r_pf=$(extract_float "$log_r" "Parallel for")
    n_cr=$(extract_float "$log_n" "Critical")
    r_cr=$(extract_float "$log_r" "Critical")

    local ratio_fj ratio_bar ratio_pf ratio_cr
    ratio_fj=$(awk "BEGIN{printf \"%.2f\", $r_fj / $n_fj}")
    ratio_bar=$(awk "BEGIN{printf \"%.2f\", $r_bar / $n_bar}")
    ratio_pf=$(awk "BEGIN{printf \"%.2f\", $r_pf / $n_pf}")
    ratio_cr=$(awk "BEGIN{printf \"%.2f\", $r_cr / $n_cr}")

    cat <<EOF
| Metric | Native libgomp | RTS-backed | Ratio |
|--------|---------------|------------|-------|
| Fork/join | $n_fj us | $r_fj us | $(format_ratio "$ratio_fj") |
| Barrier | $n_bar us | $r_bar us | $(format_ratio "$ratio_bar") |
| Parallel for (1M sin) | $n_pf ms | $r_pf ms | $(format_ratio "$ratio_pf") |
| Critical section | $n_cr ms | $r_cr ms | $(format_ratio "$ratio_cr") |
EOF
}

# ── README: DGEMM (4-thread summary, 512 and 1024 only) ─────────────

gen_readme_dgemm() {
    local log_n="$LOGDIR/bench_dgemm_native_4t.log"
    local log_r="$LOGDIR/bench_dgemm_rts_4t.log"
    [ -f "$log_n" ] || log_n="$LOGDIR/bench_dgemm_native.log"
    [ -f "$log_r" ] || log_r="$LOGDIR/bench_dgemm_rts.log"
    [ -f "$log_n" ] || return

    echo "| N | Native (ms) | RTS (ms) | Ratio |"
    echo "|---|------------|---------|-------|"
    paste <(grep -E '^\s*[0-9]+\s+[0-9]' "$log_n") \
          <(grep -E '^\s*[0-9]+\s+[0-9]' "$log_r") | \
    while read -r n1 t1 g1 c1 n2 t2 g2 c2; do
        # Only include 512 and 1024 in README
        case "$n1" in 512|1024)
            local ratio
            ratio=$(awk "BEGIN{printf \"%.2f\", $t2 / $t1}")
            echo "| $n1 | $t1 | $t2 | ${ratio}x |"
            ;;
        esac
    done
}

# ── Chart data: generate docs/ChartData.hs ────────────────────────────

gen_chart_data() {
    local out="docs/ChartData.hs"

    # --- DGEMM ---
    local log_n="$LOGDIR/bench_dgemm_native_4t.log"
    local log_r="$LOGDIR/bench_dgemm_rts_4t.log"
    [ -f "$log_n" ] || log_n="$LOGDIR/bench_dgemm_native.log"
    [ -f "$log_r" ] || log_r="$LOGDIR/bench_dgemm_rts.log"
    [ -f "$log_n" ] || { echo "  SKIP: no DGEMM logs for chart data"; return; }

    local dgemm_labels="" dgemm_data=""
    while read -r n tn tr; do
        [ -n "$dgemm_labels" ] && { dgemm_labels+=", "; dgemm_data+=", "; }
        dgemm_labels+="\"N=$n\""
        dgemm_data+="[$tn, $tr]"
    done < <(paste <(grep -E '^\s*[0-9]+\s+[0-9]' "$log_n") \
                   <(grep -E '^\s*[0-9]+\s+[0-9]' "$log_r") | \
             awk '{print $1, $2, $6}')

    # --- Crossover ---
    local cross_labels="" cross_seq="" cross_par=""
    if [ -f "$LOGDIR/crossover.log" ]; then
        while read -r e su ps; do
            [ -n "$cross_labels" ] && { cross_labels+=", "; cross_seq+=", "; cross_par+=", "; }
            case "$e" in
                1000)   cross_labels+="\"1K\"" ;;
                5000)   cross_labels+="\"5K\"" ;;
                100000) cross_labels+="\"100K\"" ;;
                *)      cross_labels+="\"$e\"" ;;
            esac
            cross_seq+="$su"
            cross_par+="$ps"
        done < <(grep -E '^\s+[0-9]+\s*\|' "$LOGDIR/crossover.log" | \
                 while IFS='|' read -r elems seq_unsafe seq_safe par_safe speedup; do
                     local e su ps
                     e=$(echo "$elems" | xargs)
                     su=$(echo "$seq_unsafe" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
                     ps=$(echo "$par_safe" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
                     case "$e" in 100|200|500|1000|5000|100000) echo "$e $su $ps" ;; esac
                 done)
    fi

    # --- FFI overhead ---
    local ffi_prim="0.0" ffi_unsafe="0.0" ffi_safe="0.0"
    if [ -f "$LOGDIR/cmm_demo.log" ]; then
        ffi_prim=$(grep "^  prim:" "$LOGDIR/cmm_demo.log" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
        ffi_unsafe=$(grep "^  unsafe:" "$LOGDIR/cmm_demo.log" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
        ffi_safe=$(grep "^  safe:" "$LOGDIR/cmm_demo.log" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
    fi

    # --- Batched calls ---
    local batch_labels="" batch_cmm="" batch_safe=""
    if [ -f "$LOGDIR/cmm_batch.log" ]; then
        while read -r b s bt; do
            [ -n "$batch_labels" ] && { batch_labels+=", "; batch_cmm+=", "; batch_safe+=", "; }
            batch_labels+="\"$b\""
            batch_cmm+="$bt"
            batch_safe+="$s"
        done < <(grep -E '^\s+[0-9]+\s*\|' "$LOGDIR/cmm_batch.log" | \
                 while IFS='|' read -r batch safe batched speedup; do
                     local b s bt
                     b=$(echo "$batch" | xargs)
                     s=$(echo "$safe" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
                     bt=$(echo "$batched" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
                     echo "$b $s $bt"
                 done)
    fi

    # --- Write the module ---
    cat > "$out" <<HEOF
-- AUTO-GENERATED by scripts/update_bench_tables.sh — do not edit
module ChartData where

-- DGEMM comparison (4 threads)
dgemmLabels :: [String]
dgemmLabels = [$dgemm_labels]
dgemmData :: [[Double]]
dgemmData = [$dgemm_data]

-- Crossover
crossoverLabels :: [String]
crossoverLabels = [$cross_labels]
crossoverSeq, crossoverPar :: [Double]
crossoverSeq = [$cross_seq]
crossoverPar = [$cross_par]

-- FFI overhead
ffiData :: [[Double]]
ffiData = [[$ffi_prim], [$ffi_unsafe], [$ffi_safe]]

-- Batched calls
batchedLabels :: [String]
batchedLabels = [$batch_labels]
batchedCmm, batchedSafe :: [Double]
batchedCmm = [$batch_cmm]
batchedSafe = [$batch_safe]
HEOF
    echo "  Generated $out"
}

# ── Main ─────────────────────────────────────────────────────────────

echo "Updating benchmark tables..."

# README
content=$(gen_readme_micro)
[ -n "$content" ] && replace_section "$README" "readme_micro" "$content"

content=$(gen_readme_dgemm)
[ -n "$content" ] && replace_section "$README" "readme_dgemm" "$content"

# Chart data module
gen_chart_data

echo "Done."
