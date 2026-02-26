#!/usr/bin/env python3
"""
preprocess_docs.py — Build-time doc preprocessor for ghc-openmp.

Auto-numbers headings, resolves symbolic cross-references, injects benchmark
data from results.json, generates TOC. Validates coherence and fails the
build on any inconsistency.

Source format:
  ## Architecture {#sec:architecture}
  See [@sec:benchmarks] for details.

Output format:
  ## 4. Architecture
  See [§9](#9-benchmarks) for details.
"""

import argparse
import json
import math
import re
import shutil
import sys
from pathlib import Path


# ═══════════════════════════════════════════════════════════════════════
# Slugify — MkDocs-compatible anchor generation
# ═══════════════════════════════════════════════════════════════════════

def slugify(text):
    """Convert heading text to MkDocs anchor (Python-Markdown's toc extension)."""
    text = text.lower()
    text = re.sub(r'[^\w\s-]', '', text)   # drop punctuation
    text = re.sub(r'\s+', '-', text.strip())
    text = re.sub(r'-+', '-', text)         # collapse multiple dashes
    return text


# ═══════════════════════════════════════════════════════════════════════
# Heading parsing
# ═══════════════════════════════════════════════════════════════════════

HEADING_RE = re.compile(
    r'^(#{2,4})\s+'           # ## / ### / ####
    r'(.+?)'                  # title (non-greedy)
    r'(?:\s+\{#(sec:\S+)\})?' # optional {#sec:label}
    r'\s*$'
)

def parse_heading(line):
    """Parse heading line → (level, title, label) or None."""
    m = HEADING_RE.match(line)
    if not m:
        return None
    return len(m.group(1)), m.group(2).strip(), m.group(3)


# ═══════════════════════════════════════════════════════════════════════
# Section data
# ═══════════════════════════════════════════════════════════════════════

class Section:
    __slots__ = ('level', 'title', 'label', 'number', 'anchor', 'is_appendix')

    def __init__(self, level, title, label, number, anchor, is_appendix=False):
        self.level = level
        self.title = title
        self.label = label
        self.number = number    # "4", "4.1", "A.1", or ""
        self.anchor = anchor    # "4-architecture"
        self.is_appendix = is_appendix

    def numbered_title(self):
        if self.number:
            if self.is_appendix and self.level == 2:
                return f"Appendix {self.number}: {self.title}"
            return f"{self.number}. {self.title}" if '.' not in self.number or self.level == 3 else f"{self.number} {self.title}"
        return self.title


# ═══════════════════════════════════════════════════════════════════════
# BENCH table generators
# ═══════════════════════════════════════════════════════════════════════

def _fmt_ratio_mt(native, rts):
    """Format ratio for multi-thread overhead tables."""
    if native == 0:
        return "—"
    ratio = rts / native
    if ratio < 0.95:
        inv = native / rts
        return f"{inv:.1f}x faster"
    return f"{ratio:.2f}"


def _fmt_speedup(val):
    """Bold speedup if >= 1.0."""
    if val >= 1.0:
        return f"**{val:.2f}x**"
    return f"{val:.2f}x"


def _fmt_elements(n):
    """Format element count: 10000 → 10K, 1000000 → 1M."""
    if n >= 1_000_000:
        return f"{n // 1_000_000}M"
    if n >= 1_000:
        return f"{n // 1_000}K"
    return str(n)


def _fmt_tasks(n):
    """Format task count with commas."""
    if n >= 1000:
        return f"{n:,}"
    return str(n)


class BenchGen:
    """Generates markdown tables from results.json data."""

    def __init__(self, data):
        self.data = data

    def generate(self, key):
        """Generate table content for a BENCH key. Returns markdown string."""
        dispatch = {
            'overhead_forkjoin': lambda: self._overhead('fork_join_us', 'Fork/Join Overhead (us/iter)'),
            'overhead_barrier':  lambda: self._overhead('barrier_us', 'Barrier Latency (us/iter)'),
            'overhead_parfor':   lambda: self._overhead('parallel_for_ms', 'Parallel For + Reduction (1M sin(), best of 10, ms)'),
            'overhead_critical': lambda: self._overhead('critical_ms', 'Critical Section (1000 lock/unlock per thread, ms)'),
            'dgemm_4t':          self._dgemm_4t,
            'dgemm_scaling':     self._dgemm_scaling,
            'crossover':         self._crossover,
            'par_compare':       self._par_compare,
            'tasks':             self._tasks,
            'calling_convention': self._calling_convention,
            'batched':           self._batched,
        }
        gen = dispatch.get(key)
        if gen is None:
            return None  # unknown key
        return gen()

    # Fields where the 1-thread RTS fast path dominates the ratio
    _FAST_PATH_FIELDS = {'fork_join_us', 'barrier_us'}

    def _overhead(self, field, title):
        by_threads = self.data.get('overhead_by_threads', {})
        has_footnote = field in self._FAST_PATH_FIELDS
        lines = [f"#### {title}", "",
                 "| Threads | Native libgomp | RTS-backed | Ratio |",
                 "|---|---|---|---|"]
        for t in ['1', '2', '4', '8']:
            entry = by_threads.get(t)
            if not entry:
                continue
            nv = entry['native'][field]
            rv = entry['rts'][field]
            ratio = _fmt_ratio_mt(nv, rv)
            if t == '1' and has_footnote:
                ratio += " *"
            lines.append(f"| {t} | {nv} | {rv} | {ratio} |")
        if has_footnote:
            lines.append("")
            lines.append("\\* *1-thread: RTS uses a single-thread fast path "
                         "(`nthreads == 1` → direct function call, no barrier "
                         "init). libgomp still performs full thread-pool setup.*")
        return "\n".join(lines) + "\n"

    def _dgemm_4t(self):
        native = self.data.get('dgemm', {}).get('native', {})
        rts = self.data.get('dgemm', {}).get('rts', {})
        lines = ["| N | Native (ms) | RTS (ms) | Ratio | GFLOPS (RTS) |",
                 "|---|---|---|---|---|"]
        for n in sorted(native.keys(), key=int):
            nt = native[n]['time_ms']
            rt = rts[n]['time_ms']
            rg = rts[n]['gflops']
            ratio = rt / nt if nt > 0 else 0
            lines.append(f"| {n} | {nt} | {rt} | {ratio:.2f}x | {rg} |")
        return "\n".join(lines) + "\n"

    def _dgemm_scaling(self):
        by_threads = self.data.get('dgemm_by_threads', {})
        lines = ["#### Scaling (RTS-backed, DGEMM 1024x1024)", "",
                 "| Threads | Time (ms) | GFLOPS | Speedup |",
                 "|---|---|---|---|"]
        t1_time = None
        for t in ['1', '2', '4']:
            entry = by_threads.get(t, {}).get('rts', {})
            row = entry.get('1024')
            if not row:
                continue
            time = row['time_ms']
            gflops = row['gflops']
            if t1_time is None:
                t1_time = time
            speedup = t1_time / time if time > 0 else 0
            lines.append(f"| {t} | {time} | {gflops} | {speedup:.1f}x |")
        return "\n".join(lines) + "\n"

    def _crossover(self):
        rows = self.data.get('crossover', [])
        subset = {100, 200, 500, 1000, 5000, 100000}
        lines = ["| Elements | Sequential | Parallel | Speedup |",
                 "|---|---|---|---|"]
        for r in rows:
            e = r['elements']
            if e not in subset:
                continue
            su = r['seq_unsafe_us']
            ps = r['par_safe_us']
            sp = r['speedup']
            sp_fmt = f"**{sp:.2f}x**" if sp >= 1.0 else f"{sp:.2f}x"
            lines.append(f"| {e} | {su} us | {ps} us | {sp_fmt} |")
        return "\n".join(lines) + "\n"

    def _par_compare(self):
        rows = self.data.get('par_compare', [])
        subset = {10000, 100000, 1000000, 10000000}
        lines = ["| Elements | Seq Haskell | Seq C | Par Haskell | Par OpenMP | Hs/OMP ratio |",
                 "|---|---|---|---|---|---|"]
        for r in rows:
            e = r['elements']
            if e not in subset:
                continue
            label = _fmt_elements(e)
            lines.append(f"| {label} | {r['seq_hs_us']} us | {r['seq_c_us']} us "
                         f"| {r['par_hs_us']} us | {r['par_omp_us']} us | {r['hs_omp_ratio']:.2f}x |")
        return "\n".join(lines) + "\n"

    def _tasks(self):
        rows = self.data.get('tasks', [])
        lines = ["| Tasks | Sequential | Parallel | Speedup |",
                 "|------:|-----------:|---------:|--------:|"]
        for r in rows:
            n = _fmt_tasks(r['tasks'])
            lines.append(f"| {n}   | {r['sequential_ms']} ms     | {r['parallel_ms']} ms   | {r['speedup']:.2f}x   |")
        return "\n".join(lines) + "\n"

    def _calling_convention(self):
        cc = self.data.get('calling_convention', {})
        unsafe = cc.get('unsafe_ns', 0)
        safe = cc.get('safe_ns', 0)
        rel = round(safe / unsafe) if unsafe > 0 else 0
        return "\n".join([
            "| Convention | ns/call | Relative | Mechanism |",
            "|---|---|---|---|",
            f"| `foreign import prim` (Cmm) | ~0 | — | Direct register read, GHC optimizes away |",
            f"| `foreign import ccall unsafe` | {cc.get('unsafe_ns', 0)} | — | Save/restore STG registers |",
            f"| `foreign import ccall safe` | {cc.get('safe_ns', 0)} | {rel}x vs unsafe | + suspendThread/resumeThread |",
        ]) + "\n"

    def _batched(self):
        rows = self.data.get('batched_safe', [])
        lines = ["| Batch size | Standard safe | Cmm batched | Speedup |",
                 "|---|---|---|---|"]
        for r in rows:
            lines.append(f"| {r['batch_size']} | {r['safe_ns']} ns | {r['batched_ns']} ns | {r['speedup']:.1f}x |")
        return "\n".join(lines) + "\n"


# ═══════════════════════════════════════════════════════════════════════
# META generators
# ═══════════════════════════════════════════════════════════════════════

def gen_meta(field, system_info, commit):
    """Generate content for a <!-- META:field --> marker."""
    if field == 'system_info':
        if not system_info:
            return None
        s = system_info
        cores = s.get('cpu_cores', '?')
        phys = cores // 2 if isinstance(cores, int) else '?'
        return (f"All benchmarks on {s.get('cpu_model', 'unknown')} "
                f"({phys}C/{cores}T), NixOS, GCC {s.get('gcc_version', '?')}, "
                f"GHC {s.get('ghc_version', '?')},\n"
                f"powersave governor. Best-of-N timing to reduce CPU frequency variance.\n")
    elif field == 'bench_date':
        if not system_info:
            return None
        return f"*Benchmarked: {system_info.get('date', 'unknown')}*\n"
    elif field == 'commit':
        return commit  # Just the hash; caller uses it in GIT_COMMIT substitution
    return None


# ═══════════════════════════════════════════════════════════════════════
# Multi-page mapping
# ═══════════════════════════════════════════════════════════════════════

PAGE_MAP = {
    "00-frontmatter.md": "index.md",
    # 01-contents.md: skipped for multi-page (sidebar handles nav)
    "02-abstract.md": "abstract.md",
    "03-motivation.md": "motivation.md",
    "04-background.md": "background.md",
    "05-architecture.md": "architecture.md",
    "06-optimization.md": "optimization.md",
    "07-haskell-integration.md": "haskell-integration.md",
    "08-low-level.md": "low-level.md",
    "09-shared-memory.md": "shared-memory.md",
    "10-benchmarks.md": "benchmarks.md",
    "11-timeline.md": "timeline.md",
    "12-bugs.md": "bugs.md",
    "13-limitations.md": "limitations.md",
    "14-related-work.md": "related-work.md",
    "15-conclusions.md": "conclusions.md",
    "A1-abi-surface.md": "appendix-abi.md",
    "A2-gomp-primer.md": "appendix-gomp.md",
    "A3-ncg-llvm.md": "appendix-ncg-llvm.md",
    "A4-rts-internals.md": "appendix-rts.md",
    "A5-barrier.md": "appendix-barrier.md",
    "A6-zero-copy.md": "appendix-zero-copy.md",
    "A7-linear-arrays.md": "appendix-linear-arrays.md",
}


# ═══════════════════════════════════════════════════════════════════════
# Preprocessor
# ═══════════════════════════════════════════════════════════════════════

class Preprocessor:
    def __init__(self, sections_dir, bench_data=None, system_info=None, commit="main"):
        self.sections_dir = Path(sections_dir)
        self.bench_data = bench_data
        self.bench_gen = BenchGen(bench_data) if bench_data else None
        self.system_info = system_info
        self.commit = commit

        self.labels = {}        # label → Section
        self.toc_sections = []  # top-level sections for TOC generation
        self.errors = []

        # Numbering state
        self._main_num = 0
        self._appendix_num = 0
        self._sub_num = 0
        self._current_parent = 0
        self._in_appendix = False

    # ── Phase 1: Scan headings, assign numbers ────────────────────────

    def scan(self):
        """Scan all section files and build the label → section map."""
        for fpath in sorted(self.sections_dir.glob("*.md")):
            fname = fpath.name
            ftype = self._file_type(fname)

            with open(fpath) as f:
                for line in f:
                    parsed = parse_heading(line)
                    if not parsed:
                        continue
                    level, title, label = parsed
                    section = self._register_heading(level, title, label, ftype)
                    if section and section.level == 2:
                        self.toc_sections.append(section)

    def _file_type(self, fname):
        if fname.startswith("00-") or fname.startswith("01-"):
            return "special"
        if fname.startswith("A"):
            return "appendix"
        return "main"

    def _register_heading(self, level, title, label, ftype):
        if ftype == "special":
            # Contents heading — no numbering
            if label:
                sec = Section(level, title, label, "", slugify(title))
                self._add_label(label, sec)
                return sec
            return None

        number = ""
        is_appendix = (ftype == "appendix")

        if level == 2:
            if ftype == "main":
                self._main_num += 1
                self._sub_num = 0
                self._current_parent = self._main_num
                self._in_appendix = False
                number = str(self._main_num)
            else:
                self._appendix_num += 1
                self._sub_num = 0
                self._in_appendix = True
                number = f"A.{self._appendix_num}"
        elif level == 3:
            if not self._in_appendix:
                self._sub_num += 1
                number = f"{self._current_parent}.{self._sub_num}"
            # Appendix subsections: no number

        # Compute anchor from numbered title
        if number:
            if is_appendix and level == 2:
                anchor = slugify(f"appendix {number} {title}")
            else:
                anchor = slugify(f"{number} {title}")
        else:
            anchor = slugify(title)

        sec = Section(level, title, label, number, anchor, is_appendix)

        if label:
            self._add_label(label, sec)

        return sec

    def _add_label(self, label, section):
        if label in self.labels:
            self.errors.append(f"Duplicate label: {label} "
                               f"('{self.labels[label].title}' and '{section.title}')")
        self.labels[label] = section

    # ── Phase 2: Process files ────────────────────────────────────────

    def process(self, output_single, output_multi=None, charts_dir=None):
        """Process all files: resolve refs, inject data, write output."""
        files = sorted(self.sections_dir.glob("*.md"))
        all_output = []
        file_outputs = {}  # fname → processed content

        for fpath in files:
            fname = fpath.name
            content = self._process_file(fpath)
            file_outputs[fname] = content
            all_output.append(content)

        # Check for errors before writing
        if self.errors:
            for e in self.errors:
                print(f"ERROR: {e}", file=sys.stderr)
            sys.exit(1)

        # Write single-page
        Path(output_single).parent.mkdir(parents=True, exist_ok=True)
        Path(output_single).write_text("".join(all_output))
        print(f"  Single-page: {output_single}")

        # Write multi-page
        if output_multi:
            self._write_multipage(file_outputs, output_multi, charts_dir)

        print(f"  {len(self.labels)} labels resolved, {len(files)} files processed")

    def _process_file(self, fpath):
        """Process one file: headings, refs, BENCH, META, TOC."""
        lines = fpath.read_text().splitlines(keepends=True)
        out = []
        in_bench = False

        for line in lines:
            # ── TOC marker ──
            if '<!-- TOC -->' in line:
                out.append(self._generate_toc() + "\n")
                continue

            # ── BENCH start ──
            m = re.match(r'<!--\s*BENCH:(\w+)\s*-->', line)
            if m:
                key = m.group(1)
                in_bench = True
                out.append(line)  # keep start marker
                bench_content = self._gen_bench(key)
                if bench_content is not None:
                    out.append(bench_content)
                continue

            # ── BENCH end ──
            m = re.match(r'<!--\s*/BENCH:(\w+)\s*-->', line)
            if m:
                in_bench = False
                out.append(line)  # keep end marker
                continue

            # ── Inside BENCH (skip existing content) ──
            if in_bench:
                continue

            # ── META marker ──
            m = re.match(r'<!--\s*META:(\w+)\s*-->', line)
            if m:
                meta = gen_meta(m.group(1), self.system_info, self.commit)
                if meta is None:
                    self.errors.append(f"Cannot resolve META:{m.group(1)}")
                else:
                    out.append(meta)
                continue

            # ── Heading ──
            parsed = parse_heading(line)
            if parsed:
                level, title, label = parsed
                out.append(self._render_heading(level, title, label))
                continue

            # ── Cross-references ──
            line = self._resolve_refs(line)

            # ── GIT_COMMIT substitution ──
            line = line.replace('GIT_COMMIT', self.commit)

            out.append(line)

        return "".join(out)

    def _render_heading(self, level, title, label):
        """Render a heading with its auto-assigned number (no {#label})."""
        hashes = '#' * level
        if label and label in self.labels:
            sec = self.labels[label]
            if sec.number:
                if sec.is_appendix and level == 2:
                    return f"{hashes} Appendix {sec.number}: {sec.title}\n"
                return f"{hashes} {sec.number}. {sec.title}\n"
        return f"{hashes} {title}\n"

    def _resolve_refs(self, line):
        """Resolve [@sec:label] references in a line."""

        def _replace(m):
            prefix = m.group(1)  # text before @sec:
            label = m.group(2)   # sec:xxx
            suffix = m.group(3)  # text after label

            if label not in self.labels:
                self.errors.append(f"Unresolved reference: @{label}")
                return m.group(0)

            sec = self.labels[label]
            display = sec.number if sec.number else sec.title

            if prefix.strip():
                # [Section @sec:foo] → [Section 4.1](#41-worker-pool-design)
                return f"[{prefix}{display}{suffix}](#{sec.anchor})"
            else:
                # [@sec:foo] → [§4](#4-architecture)
                return f"[§{display}](#{sec.anchor})"

        return re.sub(
            r'\[([^@\]]*?)@(sec:[^\s\]]+)([^\]]*?)\]',
            _replace, line
        )

    def _gen_bench(self, key):
        """Generate BENCH table content. Returns string or None."""
        if not self.bench_gen:
            self.errors.append(f"BENCH:{key} requires --bench-data")
            return ""
        content = self.bench_gen.generate(key)
        if content is None:
            self.errors.append(f"Unknown BENCH key: {key}")
            return ""
        return content

    def _generate_toc(self):
        """Generate table of contents from scanned sections."""
        lines = []
        item_num = 0
        for sec in self.toc_sections:
            item_num += 1
            if sec.is_appendix:
                lines.append(f"{item_num}. [Appendix {sec.number}: "
                             f"{sec.title}](#{sec.anchor})")
            else:
                lines.append(f"{item_num}. [{sec.title}](#{sec.anchor})")
        return "\n".join(lines)

    # ── Multi-page output ─────────────────────────────────────────────

    def _write_multipage(self, file_outputs, multi_dir, charts_dir):
        multi_dir = Path(multi_dir)
        multi_dir.mkdir(parents=True, exist_ok=True)

        # Copy charts
        if charts_dir:
            charts_src = Path(charts_dir)
            charts_dst = multi_dir / "charts"
            charts_dst.mkdir(exist_ok=True)
            for svg in charts_src.glob("*.svg"):
                shutil.copy2(svg, charts_dst / svg.name)

        for fname, content in file_outputs.items():
            page_name = PAGE_MAP.get(fname)
            if not page_name:
                continue

            if fname == "00-frontmatter.md":
                # Swap multi→single link for multi-page index
                content = content.replace(
                    "Multi-page view](pages/)",
                    "Single-page view](../)")
            elif fname != "01-contents.md":
                # Promote headings: ## → #, ### → ##, etc.
                content = re.sub(
                    r'^(#{2,}) ',
                    lambda m: '#' * (len(m.group(1)) - 1) + ' ',
                    content,
                    flags=re.MULTILINE)

            (multi_dir / page_name).write_text(content)

        print(f"  Multi-page: {multi_dir}/ ({len(PAGE_MAP)} pages)")


# ═══════════════════════════════════════════════════════════════════════
# Main
# ═══════════════════════════════════════════════════════════════════════

def main():
    parser = argparse.ArgumentParser(description="Preprocess ghc-openmp docs")
    parser.add_argument("--sections", required=True, help="docs/sections/ directory")
    parser.add_argument("--bench-data", help="artifacts/bench/results.json")
    parser.add_argument("--system-info", help="artifacts/bench/system_info.json")
    parser.add_argument("--commit", default="main", help="Git commit hash")
    parser.add_argument("--single-page", required=True, help="Output: single-page markdown")
    parser.add_argument("--multi-page", help="Output: multi-page directory")
    parser.add_argument("--charts-dir", help="Charts directory to copy to multi-page")
    args = parser.parse_args()

    bench_data = None
    if args.bench_data and Path(args.bench_data).exists():
        bench_data = json.loads(Path(args.bench_data).read_text())

    system_info = None
    if args.system_info and Path(args.system_info).exists():
        system_info = json.loads(Path(args.system_info).read_text())

    pp = Preprocessor(args.sections, bench_data, system_info, args.commit)
    pp.scan()
    pp.process(args.single_page, args.multi_page, args.charts_dir)


if __name__ == "__main__":
    main()
