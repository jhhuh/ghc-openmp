# MkDocs: Split Sections + Concatenate at Build Time

## Problem
A single long `docs/index.md` (1000+ lines) is hard to maintain but you want a single-page output for readers.

## Solution
Split into `docs/sections/NN-name.md` files. Concatenate at build time before `mkdocs build`.

## Layout
```
docs/
  sections/
    00-frontmatter.md   # Title, subtitle
    01-contents.md      # TOC with anchor links
    02-abstract.md
    ...
    A1-appendix-foo.md
    A2-appendix-bar.md
  gen_charts.hs         # other docs assets
```

Files are named with numeric prefixes so `cat docs/sections/*.md` produces the correct order. Appendices use `A1-`, `A2-` prefixes to sort after numbered sections.

## Build step (flake.nix)
```nix
buildPhase = ''
  # Concatenate sections into single index.md
  cat docs/sections/*.md > docs/index.md

  # ... rest of build (charts, substitutions, mkdocs build)
  mkdocs build -d _site
'';
```

## MkDocs config
Exclude the source sections from MkDocs so it only sees the concatenated `index.md`:
```yaml
exclude_docs: |
  sections/
```

## Cross-references
Internal `#section-slug` anchors work unchanged because the concatenated output is a single page. No changes needed to existing cross-references.

## Splitting technique
Use `grep -n '^## '` to find section boundaries, then `sed -n 'START,ENDp'` to extract each range. Verify with `diff original.md <(cat sections/*.md)`.

## Gotchas
- Don't include a trailing newline in section files that would create double blank lines at boundaries
- The `---` horizontal rules between sections should go at the END of each section file (before the next one starts)
- If using build-time substitutions (like `#FN:` anchors), run them on the concatenated `docs/index.md`, not on individual section files
