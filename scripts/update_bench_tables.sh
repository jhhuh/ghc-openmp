#!/usr/bin/env bash
# Update benchmark tables in README.md and docs/sections/09-benchmarks.md
# from artifacts/bench/ data (logs + results.json).
#
# Tables are delimited by <!-- BENCH:key --> / <!-- /BENCH:key --> markers.
# Content between markers is replaced; everything else is preserved.
#
# Usage: scripts/update_bench_tables.sh [bench_dir]
#   bench_dir defaults to artifacts/bench

set -euo pipefail

BENCH="${1:-artifacts/bench}"
LOGDIR="$BENCH/logs"
JSON="$BENCH/results.json"
README="README.md"
DOCS="docs/sections/09-benchmarks.md"

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

# Format ratio for multi-thread tables
format_ratio_mt() {
    local native="$1" rts="$2"
    local ratio
    ratio=$(awk "BEGIN{printf \"%.2f\", $rts / $native}")
    if awk "BEGIN{exit !($ratio < 0.95)}" 2>/dev/null; then
        local inv
        inv=$(awk "BEGIN{printf \"%.1f\", $native / $rts}")
        printf "%sx faster" "$inv"
    else
        printf "%s" "$ratio"
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

# ── Docs: multi-thread overhead tables ───────────────────────────────

gen_overhead_table() {
    local metric_grep="$1" title="$2" unit="$3"
    local threads=(1 2 4 8)
    echo "#### $title"
    echo ""
    echo "| Threads | Native libgomp | RTS-backed | Ratio |"
    echo "|---|---|---|---|"
    for t in "${threads[@]}"; do
        local log_n="$LOGDIR/bench_overhead_native_${t}t.log"
        local log_r="$LOGDIR/bench_overhead_rts_${t}t.log"
        [ -f "$log_n" ] || continue
        local nv rv
        nv=$(extract_float "$log_n" "$metric_grep")
        rv=$(extract_float "$log_r" "$metric_grep")
        [ -n "$nv" ] && [ -n "$rv" ] || continue
        printf "| %s | %s | %s | %s |\n" "$t" "$nv" "$rv" "$(format_ratio_mt "$nv" "$rv")"
    done
}

# ── Docs: DGEMM 4-thread table ───────────────────────────────────────

gen_dgemm_4t() {
    local log_n="$LOGDIR/bench_dgemm_native_4t.log"
    local log_r="$LOGDIR/bench_dgemm_rts_4t.log"
    [ -f "$log_n" ] || log_n="$LOGDIR/bench_dgemm_native.log"
    [ -f "$log_r" ] || log_r="$LOGDIR/bench_dgemm_rts.log"
    [ -f "$log_n" ] || return

    echo "| N | Native (ms) | RTS (ms) | Ratio | GFLOPS (RTS) |"
    echo "|---|---|---|---|---|"
    paste <(grep -E '^\s*[0-9]+\s+[0-9]' "$log_n") \
          <(grep -E '^\s*[0-9]+\s+[0-9]' "$log_r") | \
    while read -r n1 t1 g1 c1 n2 t2 g2 c2; do
        local ratio
        ratio=$(awk "BEGIN{printf \"%.2f\", $t2 / $t1}")
        echo "| $n1 | $t1 | $t2 | ${ratio}x | $g2 |"
    done
}

# ── Docs: DGEMM scaling (RTS only, 1024x1024, multiple threads) ─────

gen_dgemm_scaling() {
    echo "#### Scaling (RTS-backed, DGEMM 1024x1024)"
    echo ""
    echo "| Threads | Time (ms) | GFLOPS | Speedup |"
    echo "|---|---|---|---|"
    local t1_time=""
    for t in 1 2 4; do
        local log="$LOGDIR/bench_dgemm_rts_${t}t.log"
        [ -f "$log" ] || continue
        local row
        row=$(grep -E '^\s*1024\s' "$log" | head -1)
        [ -n "$row" ] || continue
        local time gflops
        time=$(echo "$row" | awk '{print $2}')
        gflops=$(echo "$row" | awk '{print $3}')
        [ -z "$t1_time" ] && t1_time="$time"
        local speedup
        speedup=$(awk "BEGIN{printf \"%.1f\", $t1_time / $time}")
        echo "| $t | $time | $gflops | ${speedup}x |"
    done
}

# ── Docs: crossover table ────────────────────────────────────────────

gen_crossover() {
    local log="$LOGDIR/crossover.log"
    [ -f "$log" ] || return
    echo "| Elements | Sequential | Parallel | Speedup |"
    echo "|---|---|---|---|"
    grep -E '^\s+[0-9]+\s*\|' "$log" | while IFS='|' read -r elems seq_unsafe seq_safe par_safe speedup; do
        local e su ps sp
        e=$(echo "$elems" | xargs)
        su=$(echo "$seq_unsafe" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
        ps=$(echo "$par_safe" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
        sp=$(echo "$speedup" | grep -oE '[0-9]+\.[0-9]+')
        # Only include a representative subset for the docs table
        case "$e" in 100|200|500|1000|5000|100000)
            if awk "BEGIN{exit !($sp >= 1.0)}" 2>/dev/null; then
                printf "| %s | %s us | %s us | **%sx** |\n" "$e" "$su" "$ps" "$sp"
            else
                printf "| %s | %s us | %s us | %sx |\n" "$e" "$su" "$ps" "$sp"
            fi
            ;;
        esac
    done
}

# ── Docs: par_compare NCG table ──────────────────────────────────────

gen_par_compare() {
    local log="$LOGDIR/par_compare.log"
    [ -f "$log" ] || return
    echo "| Elements | Seq Haskell | Seq C | Par Haskell | Par OpenMP | Hs/OMP ratio |"
    echo "|---|---|---|---|---|---|"
    grep -E '^\s+[0-9]+\s*\|' "$log" | while IFS='|' read -r elems seq_hs seq_c par_hs par_omp hs_omp c_omp; do
        local e shs sc phs pomp hr
        e=$(echo "$elems" | xargs)
        shs=$(echo "$seq_hs" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
        sc=$(echo "$seq_c" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
        phs=$(echo "$par_hs" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
        pomp=$(echo "$par_omp" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
        hr=$(echo "$hs_omp" | grep -oE '[0-9]+\.[0-9]+')
        # Show 10K, 100K, 1M, 10M
        case "$e" in 10000|100000|1000000|10000000)
            local label
            if [ "$e" -ge 1000000 ]; then
                label="$((e / 1000000))M"
            else
                label="$((e / 1000))K"
            fi
            printf "| %s | %s us | %s us | %s us | %s us | %sx |\n" \
                "$label" "$shs" "$sc" "$phs" "$pomp" "$hr"
            ;;
        esac
    done
}

# ── Docs: task execution table ───────────────────────────────────────

gen_tasks() {
    local log="$LOGDIR/task_demo.log"
    [ -f "$log" ] || return
    echo "| Tasks | Sequential | Parallel | Speedup |"
    echo "|------:|-----------:|---------:|--------:|"
    grep -E '^\s+[0-9]+\s*\|' "$log" | while IFS='|' read -r ntasks seq_ms par_ms speedup; do
        local n s p sp
        n=$(echo "$ntasks" | xargs)
        s=$(echo "$seq_ms" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
        p=$(echo "$par_ms" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
        sp=$(echo "$speedup" | grep -oE '[0-9]+\.[0-9]+')
        # Format with comma for thousands
        local nfmt="$n"
        case "$n" in
            1000)  nfmt="1,000" ;;
            5000)  nfmt="5,000" ;;
            10000) nfmt="10,000" ;;
        esac
        printf "| %s   | %s ms     | %s ms   | %sx   |\n" "$nfmt" "$s" "$p" "$sp"
    done
}

# ── Docs: calling convention table ───────────────────────────────────

gen_calling_convention() {
    local log="$LOGDIR/cmm_demo.log"
    [ -f "$log" ] || return
    local prim unsafe safe
    prim=$(grep "^  prim:" "$log" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
    unsafe=$(grep "^  unsafe:" "$log" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
    safe=$(grep "^  safe:" "$log" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
    local rel
    rel=$(awk "BEGIN{printf \"%.0f\", $safe / $unsafe}")
    cat <<EOF
| Convention | ns/call | Relative | Mechanism |
|---|---|---|---|
| \`foreign import prim\` (Cmm) | ~0 | — | Direct register read, GHC optimizes away |
| \`foreign import ccall unsafe\` | $unsafe | — | Save/restore STG registers |
| \`foreign import ccall safe\` | $safe | ${rel}x vs unsafe | + suspendThread/resumeThread |
EOF
}

# ── Docs: batched calls table ────────────────────────────────────────

gen_batched() {
    local log="$LOGDIR/cmm_batch.log"
    [ -f "$log" ] || return
    echo "| Batch size | Standard safe | Cmm batched | Speedup |"
    echo "|---|---|---|---|"
    grep -E '^\s+[0-9]+\s*\|' "$log" | while IFS='|' read -r batch safe batched speedup; do
        local b s bt sp
        b=$(echo "$batch" | xargs)
        s=$(echo "$safe" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
        bt=$(echo "$batched" | grep -oE '[0-9]+\.?[0-9]*' | head -1)
        sp=$(echo "$speedup" | grep -oE '[0-9]+\.[0-9]+')
        printf "| %s | %s ns | %s ns | %sx |\n" "$b" "$s" "$bt" "$sp"
    done
}

# ── Main ─────────────────────────────────────────────────────────────

echo "Updating benchmark tables..."

# README
content=$(gen_readme_micro)
[ -n "$content" ] && replace_section "$README" "readme_micro" "$content"

content=$(gen_readme_dgemm)
[ -n "$content" ] && replace_section "$README" "readme_dgemm" "$content"

# Docs: overhead tables (need multi-thread logs)
if [ -f "$LOGDIR/bench_overhead_native_1t.log" ]; then
    content=$(gen_overhead_table "Fork/join" "Fork/Join Overhead (us/iter)" "us")
    [ -n "$content" ] && replace_section "$DOCS" "overhead_forkjoin" "$content"

    content=$(gen_overhead_table "Barrier" "Barrier Latency (us/iter)" "us")
    [ -n "$content" ] && replace_section "$DOCS" "overhead_barrier" "$content"

    content=$(gen_overhead_table "Parallel for" "Parallel For + Reduction (1M sin(), best of 10, ms)" "ms")
    [ -n "$content" ] && replace_section "$DOCS" "overhead_parfor" "$content"

    content=$(gen_overhead_table "Critical" "Critical Section (1000 lock/unlock per thread, ms)" "ms")
    [ -n "$content" ] && replace_section "$DOCS" "overhead_critical" "$content"
fi

# Docs: DGEMM
content=$(gen_dgemm_4t)
[ -n "$content" ] && replace_section "$DOCS" "dgemm_4t" "$content"

if [ -f "$LOGDIR/bench_dgemm_rts_1t.log" ]; then
    content=$(gen_dgemm_scaling)
    [ -n "$content" ] && replace_section "$DOCS" "dgemm_scaling" "$content"
fi

# Docs: crossover, par_compare, tasks, calling convention, batched
content=$(gen_crossover)
[ -n "$content" ] && replace_section "$DOCS" "crossover" "$content"

content=$(gen_par_compare)
[ -n "$content" ] && replace_section "$DOCS" "par_compare" "$content"

content=$(gen_tasks)
[ -n "$content" ] && replace_section "$DOCS" "tasks" "$content"

content=$(gen_calling_convention)
[ -n "$content" ] && replace_section "$DOCS" "calling_convention" "$content"

content=$(gen_batched)
[ -n "$content" ] && replace_section "$DOCS" "batched" "$content"

echo "Done."
