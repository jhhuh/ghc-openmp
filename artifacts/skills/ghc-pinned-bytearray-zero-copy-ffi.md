# Zero-copy FFI with pinned ByteArray# and unboxed primops

## Pattern

Use `newPinnedByteArray#` + `mutableByteArrayContents#` for zero-copy FFI.
`readDoubleArray#`/`writeDoubleArray#` eliminate CDouble boxing.

## Key points

- `mutableByteArrayContents#` gives C a direct pointer (no malloc/copy)
- Must be pinned so GC doesn't relocate
- `touch#` keeps ByteArray alive during C call
- Verify with `-ddump-simpl`: no `D#` constructor in hot path
- `forM_ [0..n-1]` allocates — use manual `go` loop instead

## Measured improvement

Inner loop 19% faster at N=512 (DGEMM).
