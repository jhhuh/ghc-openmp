#!/usr/bin/env bash
# Run all benchmarks, collect system info, save logs and parsed results.
# Usage: nix run .#benchmark
#   or:  scripts/run_benchmarks.sh <bindir>
set -euo pipefail

BINDIR="${1:?Usage: $0 <bindir>}"
OUTDIR="artifacts/bench"
LOGDIR="$OUTDIR/logs"
mkdir -p "$LOGDIR"

THREADS=4

# ============================================================
# 1. System info
# ============================================================
collect_sysinfo() {
    local cpu_model cores kernel ghc_ver gcc_ver
    cpu_model=$(grep -m1 'model name' /proc/cpuinfo 2>/dev/null | cut -d: -f2 | xargs || echo "unknown")
    cores=$(nproc 2>/dev/null || echo "unknown")
    kernel=$(uname -r 2>/dev/null || echo "unknown")
    ghc_ver=$(ghc --numeric-version 2>/dev/null || echo "unknown")
    gcc_ver=$(gcc -dumpversion 2>/dev/null || echo "unknown")

    cat > "$OUTDIR/system_info.json" <<EOF
{
  "cpu_model": "$cpu_model",
  "cpu_cores": $cores,
  "kernel": "$kernel",
  "ghc_version": "$ghc_ver",
  "gcc_version": "$gcc_ver",
  "omp_threads": $THREADS,
  "date": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "hostname": "$(hostname 2>/dev/null || echo unknown)"
}
EOF
    echo "  -> $OUTDIR/system_info.json"
}

# ============================================================
# 2. Run benchmarks and capture logs
# ============================================================
run_bench() {
    local name="$1"
    local cmd="$2"
    local log="$LOGDIR/$name.log"
    echo "  Running: $name"
    eval "$cmd" > "$log" 2>&1 || true
    echo "    -> $log"
}

run_all_benchmarks() {
    local p="$BINDIR"

    # C microbenchmarks at multiple thread counts
    for t in 1 2 4 8; do
        run_bench "bench_overhead_native_${t}t" "OMP_NUM_THREADS=$t $p/bench_native"
        run_bench "bench_overhead_rts_${t}t"    "OMP_NUM_THREADS=$t $p/bench_rts"
    done

    # DGEMM at multiple thread counts (1-4, matching scaling table)
    for t in 1 2 4; do
        run_bench "bench_dgemm_native_${t}t" "OMP_NUM_THREADS=$t $p/bench_dgemm_native"
        run_bench "bench_dgemm_rts_${t}t"    "OMP_NUM_THREADS=$t $p/bench_dgemm_rts"
    done

    # Haskell benchmarks
    run_bench "par_compare"  "$p/par_compare +RTS -N$THREADS"
    run_bench "cmm_demo"     "$p/cmm_demo +RTS -N$THREADS"
    run_bench "cmm_batch"    "$p/cmm_batch +RTS -N$THREADS"
    run_bench "crossover"    "$p/crossover +RTS -N$THREADS"
    run_bench "task_demo"    "$p/task_demo +RTS -N$THREADS"

    # C task tests
    run_bench "test_tasks_native" "OMP_NUM_THREADS=$THREADS $p/test_tasks_native"
    run_bench "test_tasks_rts"    "OMP_NUM_THREADS=$THREADS $p/test_tasks_rts"
}

# ============================================================
# 3. Parse logs -> JSON
# ============================================================

# Extract a float after a pattern from a log file
extract_float() {
    local file="$1" pattern="$2"
    grep "$pattern" "$file" | head -1 | grep -oE '[0-9]+\.[0-9]+' | head -1
}

parse_overhead() {
    local file="$1" prefix="$2"
    local fj bar pf cr tk
    fj=$(extract_float "$file" "Fork/join")
    bar=$(extract_float "$file" "Barrier")
    pf=$(extract_float "$file" "Parallel for")
    cr=$(extract_float "$file" "Critical")
    tk=$(extract_float "$file" "Tasks")
    echo "    \"$prefix\": { \"fork_join_us\": $fj, \"barrier_us\": $bar, \"parallel_for_ms\": $pf, \"critical_ms\": $cr, \"tasks_ms\": $tk }"
}

parse_dgemm() {
    local file="$1" prefix="$2"
    echo "    \"$prefix\": {"
    local first=true
    # Data rows: N  Time  GFLOPS  Checksum (skip header)
    grep -E '^\s*[0-9]+\s+[0-9]' "$file" | while read -r n time gflops chk; do
        if [ "$first" = true ]; then first=false; else echo ","; fi
        printf '      "%s": { "time_ms": %s, "gflops": %s }' "$n" "$time" "$gflops"
    done
    echo ""
    echo "    }"
}

parse_par_compare() {
    echo '  "par_compare": ['
    local first=true
    # Table rows with | separator, us values
    grep -E '^\s+[0-9]+\s*\|' "$LOGDIR/par_compare.log" | while IFS='|' read -r elems seq_hs seq_c par_hs par_omp hs_omp c_omp; do
        local e=$(echo "$elems" | xargs)
        local shs=$(echo "$seq_hs" | grep -oE '[0-9]+\.[0-9]+')
        local sc=$(echo "$seq_c" | grep -oE '[0-9]+\.[0-9]+')
        local phs=$(echo "$par_hs" | grep -oE '[0-9]+\.[0-9]+')
        local pomp=$(echo "$par_omp" | grep -oE '[0-9]+\.[0-9]+')
        local hr=$(echo "$hs_omp" | grep -oE '[0-9]+\.[0-9]+')
        local cr=$(echo "$c_omp" | grep -oE '[0-9]+\.[0-9]+')
        if [ "$first" = true ]; then first=false; else echo ","; fi
        printf '    { "elements": %s, "seq_hs_us": %s, "seq_c_us": %s, "par_hs_us": %s, "par_omp_us": %s, "hs_omp_ratio": %s, "c_omp_ratio": %s }' \
            "$e" "$shs" "$sc" "$phs" "$pomp" "$hr" "$cr"
    done
    echo ""
    echo '  ]'
}

parse_calling_convention() {
    local file="$LOGDIR/cmm_demo.log"
    local prim unsafe safe
    prim=$(grep "^  prim:" "$file" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
    unsafe=$(grep "^  unsafe:" "$file" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
    safe=$(grep "^  safe:" "$file" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
    echo "  \"calling_convention\": { \"prim_ns\": $prim, \"unsafe_ns\": $unsafe, \"safe_ns\": $safe }"
}

parse_batched_safe() {
    echo '  "batched_safe": ['
    local first=true
    # Table rows: batch_size | safe_ns | batched_ns | speedup
    grep -E '^\s+[0-9]+\s*\|' "$LOGDIR/cmm_batch.log" | while IFS='|' read -r batch safe batched speedup; do
        local b=$(echo "$batch" | xargs)
        local s=$(echo "$safe" | grep -oE '[0-9]+\.[0-9]+')
        local bt=$(echo "$batched" | grep -oE '[0-9]+\.[0-9]+')
        local sp=$(echo "$speedup" | grep -oE '[0-9]+\.[0-9]+')
        if [ "$first" = true ]; then first=false; else echo ","; fi
        printf '    { "batch_size": %s, "safe_ns": %s, "batched_ns": %s, "speedup": %s }' "$b" "$s" "$bt" "$sp"
    done
    echo ""
    echo '  ]'
}

parse_crossover() {
    echo '  "crossover": ['
    local first=true
    grep -E '^\s+[0-9]+\s*\|' "$LOGDIR/crossover.log" | while IFS='|' read -r elems seq_unsafe seq_safe par_safe speedup; do
        local e=$(echo "$elems" | xargs)
        local su=$(echo "$seq_unsafe" | grep -oE '[0-9]+\.[0-9]+')
        local ss=$(echo "$seq_safe" | grep -oE '[0-9]+\.[0-9]+')
        local ps=$(echo "$par_safe" | grep -oE '[0-9]+\.[0-9]+')
        local sp=$(echo "$speedup" | grep -oE '[0-9]+\.[0-9]+')
        if [ "$first" = true ]; then first=false; else echo ","; fi
        printf '    { "elements": %s, "seq_unsafe_us": %s, "seq_safe_us": %s, "par_safe_us": %s, "speedup": %s }' \
            "$e" "$su" "$ss" "$ps" "$sp"
    done
    echo ""
    echo '  ]'
}

parse_tasks() {
    echo '  "tasks": ['
    local first=true
    grep -E '^\s+[0-9]+\s*\|' "$LOGDIR/task_demo.log" | while IFS='|' read -r ntasks seq_ms par_ms speedup; do
        local n=$(echo "$ntasks" | xargs)
        local s=$(echo "$seq_ms" | grep -oE '[0-9]+\.[0-9]+')
        local p=$(echo "$par_ms" | grep -oE '[0-9]+\.[0-9]+')
        local sp=$(echo "$speedup" | grep -oE '[0-9]+\.[0-9]+')
        if [ "$first" = true ]; then first=false; else echo ","; fi
        printf '    { "tasks": %s, "sequential_ms": %s, "parallel_ms": %s, "speedup": %s }' \
            "$n" "$s" "$p" "$sp"
    done
    echo ""
    echo '  ]'
}

parse_task_tests() {
    local file="$1" prefix="$2"
    local seq_ms par_ms speedup
    seq_ms=$(grep "Sequential:" "$file" | grep -oE '[0-9]+\.[0-9]+')
    par_ms=$(grep "Tasks (" "$file" | grep -oE '[0-9]+\.[0-9]+' | head -1)
    speedup=$(grep "Speedup:" "$file" | grep -oE '[0-9]+\.[0-9]+')
    echo "    \"$prefix\": { \"sequential_ms\": $seq_ms, \"parallel_ms\": $par_ms, \"speedup\": $speedup }"
}

build_json() {
    {
        echo "{"
        # Default 4-thread overhead (backward compat)
        echo '  "overhead": {'
        parse_overhead "$LOGDIR/bench_overhead_native_4t.log" "native"
        echo ","
        parse_overhead "$LOGDIR/bench_overhead_rts_4t.log" "rts"
        echo ""
        echo "  },"
        # Multi-thread overhead
        echo '  "overhead_by_threads": {'
        local first_t=true
        for t in 1 2 4 8; do
            [ -f "$LOGDIR/bench_overhead_native_${t}t.log" ] || continue
            if [ "$first_t" = true ]; then first_t=false; else echo ","; fi
            echo "    \"${t}\": {"
            parse_overhead "$LOGDIR/bench_overhead_native_${t}t.log" "native"
            echo ","
            parse_overhead "$LOGDIR/bench_overhead_rts_${t}t.log" "rts"
            echo "    }"
        done
        echo ""
        echo "  },"
        # Default 4-thread DGEMM (backward compat)
        echo '  "dgemm": {'
        parse_dgemm "$LOGDIR/bench_dgemm_native_4t.log" "native"
        echo ","
        parse_dgemm "$LOGDIR/bench_dgemm_rts_4t.log" "rts"
        echo ""
        echo "  },"
        # Multi-thread DGEMM
        echo '  "dgemm_by_threads": {'
        local first_d=true
        for t in 1 2 4; do
            [ -f "$LOGDIR/bench_dgemm_native_${t}t.log" ] || continue
            if [ "$first_d" = true ]; then first_d=false; else echo ","; fi
            echo "    \"${t}\": {"
            parse_dgemm "$LOGDIR/bench_dgemm_native_${t}t.log" "native"
            echo ","
            parse_dgemm "$LOGDIR/bench_dgemm_rts_${t}t.log" "rts"
            echo "    }"
        done
        echo ""
        echo "  },"
        parse_par_compare
        echo ","
        parse_calling_convention
        echo ","
        parse_batched_safe
        echo ","
        parse_crossover
        echo ","
        parse_tasks
        echo ","
        echo '  "task_tests": {'
        parse_task_tests "$LOGDIR/test_tasks_native.log" "native"
        echo ","
        parse_task_tests "$LOGDIR/test_tasks_rts.log" "rts"
        echo ""
        echo "  }"
        echo "}"
    } > "$OUTDIR/results.json"
    echo "  -> $OUTDIR/results.json"
}

# ============================================================
# 4. Generate markdown summary
# ============================================================
generate_summary() {
    local json="$OUTDIR/results.json"
    {
        echo "# Benchmark Results"
        echo ""
        echo "Generated: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
        echo ""
        # System info
        echo "**System**: $(grep cpu_model "$OUTDIR/system_info.json" | cut -d'"' -f4)"
        echo "**Threads**: $THREADS | **GHC**: $(grep ghc_version "$OUTDIR/system_info.json" | cut -d'"' -f4) | **GCC**: $(grep gcc_version "$OUTDIR/system_info.json" | cut -d'"' -f4)"
        echo ""

        # Microbenchmarks table
        echo "## Microbenchmarks ($THREADS threads)"
        echo ""
        echo "| Metric | Native libgomp | RTS-backed | Ratio |"
        echo "|--------|---------------|------------|-------|"

        # Read values from log files directly (simpler than JSON parsing in bash)
        local n_fj r_fj n_bar r_bar n_pf r_pf n_cr r_cr n_tk r_tk
        n_fj=$(extract_float "$LOGDIR/bench_overhead_native_4t.log" "Fork/join")
        r_fj=$(extract_float "$LOGDIR/bench_overhead_rts_4t.log" "Fork/join")
        n_bar=$(extract_float "$LOGDIR/bench_overhead_native_4t.log" "Barrier")
        r_bar=$(extract_float "$LOGDIR/bench_overhead_rts_4t.log" "Barrier")
        n_pf=$(extract_float "$LOGDIR/bench_overhead_native_4t.log" "Parallel for")
        r_pf=$(extract_float "$LOGDIR/bench_overhead_rts_4t.log" "Parallel for")
        n_cr=$(extract_float "$LOGDIR/bench_overhead_native_4t.log" "Critical")
        r_cr=$(extract_float "$LOGDIR/bench_overhead_rts_4t.log" "Critical")
        n_tk=$(extract_float "$LOGDIR/bench_overhead_native_4t.log" "Tasks")
        r_tk=$(extract_float "$LOGDIR/bench_overhead_rts_4t.log" "Tasks")

        printf "| Fork/join | %s us | %s us | %sx |\n" "$n_fj" "$r_fj" "$(awk "BEGIN{printf \"%.2f\", $r_fj / $n_fj}")"
        printf "| Barrier | %s us | %s us | %sx |\n" "$n_bar" "$r_bar" "$(awk "BEGIN{printf \"%.2f\", $r_bar / $n_bar}")"
        printf "| Parallel for (1M sin) | %s ms | %s ms | %sx |\n" "$n_pf" "$r_pf" "$(awk "BEGIN{printf \"%.2f\", $r_pf / $n_pf}")"
        printf "| Critical (1000/thread) | %s ms | %s ms | %sx |\n" "$n_cr" "$r_cr" "$(awk "BEGIN{printf \"%.2f\", $r_cr / $n_cr}")"
        printf "| Tasks (10000) | %s ms | %s ms | %sx |\n" "$n_tk" "$r_tk" "$(awk "BEGIN{printf \"%.2f\", $r_tk / $n_tk}")"

        echo ""

        # DGEMM table
        echo "## DGEMM ($THREADS threads)"
        echo ""
        echo "| N | Native (ms) | RTS (ms) | Ratio |"
        echo "|---|------------|---------|-------|"
        paste <(grep -E '^\s*[0-9]+\s+[0-9]' "$LOGDIR/bench_dgemm_native_4t.log") \
              <(grep -E '^\s*[0-9]+\s+[0-9]' "$LOGDIR/bench_dgemm_rts_4t.log") | \
        while read -r n1 t1 g1 c1 n2 t2 g2 c2; do
            local ratio=$(awk "BEGIN{printf \"%.2f\", $t2 / $t1}")
            printf "| %s | %s | %s | %sx |\n" "$n1" "$t1" "$t2" "$ratio"
        done

        echo ""

        # Calling convention table
        echo "## Calling Convention Overhead"
        echo ""
        echo "| Convention | ns/call |"
        echo "|---|---|"
        local prim unsafe safe
        prim=$(grep "^  prim:" "$LOGDIR/cmm_demo.log" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
        unsafe=$(grep "^  unsafe:" "$LOGDIR/cmm_demo.log" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
        safe=$(grep "^  safe:" "$LOGDIR/cmm_demo.log" | grep "ns/call" | grep -oE '[0-9]+\.[0-9]+' | head -1)
        printf "| foreign import prim (Cmm) | %s |\n" "$prim"
        printf "| foreign import ccall unsafe | %s |\n" "$unsafe"
        printf "| foreign import ccall safe | %s |\n" "$safe"

        echo ""

        # Batched safe calls table
        echo "## Batched Safe Calls"
        echo ""
        echo "| Batch size | Per-call cost (ns) | Speedup vs safe |"
        echo "|---|---|---|"
        grep -E '^\s+[0-9]+\s*\|' "$LOGDIR/cmm_batch.log" | while IFS='|' read -r batch safe_ns batched_ns speedup; do
            local b=$(echo "$batch" | xargs)
            local bt=$(echo "$batched_ns" | grep -oE '[0-9]+\.[0-9]+')
            local sp=$(echo "$speedup" | grep -oE '[0-9]+\.[0-9]+')
            printf "| %s | %s | %sx |\n" "$b" "$bt" "$sp"
        done

    } > "$OUTDIR/summary.md"
    echo "  -> $OUTDIR/summary.md"
}

# ============================================================
# Main
# ============================================================
echo "=== Benchmark Suite ==="
echo ""
echo "Collecting system info..."
collect_sysinfo

echo ""
echo "Running benchmarks (threads: 1,2,4,8 for overhead; 1,2,4 for DGEMM; $THREADS for Haskell)..."
run_all_benchmarks

echo ""
echo "Parsing results..."
build_json

echo ""
echo "Generating summary..."
generate_summary

echo ""
echo "Updating README and docs tables..."
SCRIPTDIR="$(cd "$(dirname "$0")" && pwd)"
"$SCRIPTDIR/update_bench_tables.sh" "$OUTDIR"

echo ""
echo "=== Done ==="
echo "Results in: $OUTDIR/"
echo "  system_info.json  - System configuration"
echo "  results.json      - Parsed benchmark data"
echo "  summary.md        - Markdown tables for docs"
echo "  logs/             - Raw benchmark output"
echo "  README.md and docs/sections/09-benchmarks.md updated"
