# Auto-Numbering and Build-Time Data Injection for MkDocs Docs

## Problem

Manual section numbering and cross-references in markdown docs are fragile:
inserting a section requires renumbering everything downstream and updating
all `[§X](#x-anchor)` references. Manual benchmark tables can drift from
actual benchmark data. System metadata (CPU, date, commit hash) is hardcoded.

## Solution: Custom Python Preprocessor

A single-file Python preprocessor (`scripts/preprocess_docs.py`, ~400 lines,
stdlib only) that runs at `nix build` time. It handles:

1. **Auto-numbered headings**: Source files have unnumbered headings with labels;
   preprocessor assigns §1-§14 for main sections, A.1-A.7 for appendices
2. **Symbolic cross-references**: `[@sec:label]` → `[§4](#4-architecture)`
3. **Benchmark data injection**: `<!-- BENCH:key -->` markers filled from results.json
4. **System metadata injection**: `<!-- META:system_info -->` filled from system_info.json
5. **TOC generation**: `<!-- TOC -->` marker replaced with numbered section list
6. **Multi-page output**: Splits and promotes headings for multi-page MkDocs site
7. **Coherence validation**: Build fails on unresolved refs, missing data, duplicate labels

### Why not pandoc-crossref?

Investigated but rejected because:
1. **Anchor mismatch**: pandoc-crossref generates `{#sec:label}` IDs, but MkDocs
   generates anchors from heading text (e.g., `#4-architecture`). All links break.
2. **Appendix numbering**: Numbers sequentially (§15-§21) instead of A.1-A.7.
3. **Table mangling**: Converts pipe tables to simple_tables by default.
4. **Extra deps**: pandoc + pandoc-crossref are large Haskell packages.

## Source File Format

```markdown
## Architecture {#sec:architecture}

### Worker Pool Design {#sec:worker-pool}

See [@sec:benchmarks] for performance data and
[Section @sec:ffi-calling-convention] for details.

<!-- META:system_info -->

<!-- BENCH:overhead_forkjoin -->
<!-- /BENCH:overhead_forkjoin -->

<!-- TOC -->
```

## Output Format

```markdown
## 4. Architecture

### 4.1. Worker Pool Design

See [§9](#9-benchmarks) for performance data and
[Section 6.1](#61-ffi-calling-convention) for details.

All benchmarks on an Intel i7-10750H (6C/12T), ...

<!-- BENCH:overhead_forkjoin -->
#### Fork/Join Overhead (us/iter)
| Threads | Native libgomp | RTS-backed | Ratio |
...
<!-- /BENCH:overhead_forkjoin -->

1. [Abstract](#1-abstract)
2. [Motivation](#2-motivation)
...
```

## Key Implementation Details

### Heading numbering scheme
- Files `02-*.md` through `15-*.md` → main sections §1-§14
- Files `A1-*.md` through `A7-*.md` → appendices A.1-A.7
- `###` headings → subsection numbers (e.g., §4.1, §4.2) in main sections only
- `####` headings → not numbered
- `00-frontmatter.md` and `01-contents.md` → special (no numbering)

### Anchor generation
MkDocs uses Python-Markdown's `toc` extension which generates anchors from
heading text: lowercase, spaces→hyphens, drop punctuation. The preprocessor
computes anchors from the *numbered* heading text to match what MkDocs will
produce. E.g., `## 4. Architecture` → `#4-architecture`.

For appendices: `## Appendix A.1: Implemented ABI Surface` →
`#appendix-a1-implemented-abi-surface`.

### Cross-reference regex
```python
re.sub(r'\[([^@\]]*?)@(sec:[^\s\]]+)([^\]]*?)\]', _replace, line)
```
**Gotcha**: Using `\S+?` (non-greedy) for the label causes it to match just one
character. Must use `[^\s\]]+` (greedy, stops at whitespace or `]`).

### BENCH data injection
The preprocessor reads `artifacts/bench/results.json` and generates markdown
tables for 11 benchmark keys. Content between `<!-- BENCH:key -->` and
`<!-- /BENCH:key -->` markers is replaced at build time.

Source files contain empty markers only (no stale data to go out of sync).

### Build pipeline (flake.nix)
```
docs/sections/*.md
  → resolve_fn_anchors (line number anchors for source links)
  → preprocess_docs.py --sections --bench-data --system-info --commit
    → docs/index.md (single-page, auto-numbered, data injected)
    → docs/pages/*.md (multi-page, headings promoted)
  → mkdocs build (single-page)
  → mkdocs build -f mkdocs-multi.yml (multi-page)
```

No extra dependencies beyond what MkDocs already provides (Python).

## Validation (build fails on)

1. Unresolved `[@sec:...]` reference (typo in label)
2. Duplicate `{#sec:label}` labels
3. `<!-- BENCH:key -->` with no corresponding data in results.json
4. `<!-- META:field -->` with no corresponding data in system_info.json

## Related

- `nix-benchmark-runner-with-log-and-json-output.md` — benchmark data pipeline
- `scripts/update_bench_tables.sh` — README table updates (still source-edited)
- `scripts/run_benchmarks.sh` — runs benchmarks, produces results.json
