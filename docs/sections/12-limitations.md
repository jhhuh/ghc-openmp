## 11. Limitations

| Limitation | Impact | Notes |
|---|---|---|
| Serialized nesting only | Low | Inner parallel regions execute with 1 thread. True nested parallelism (multiple active levels) is not supported. |
| Single global team | Low | No support for different thread counts in nested teams. |
| No target offloading | None | Not applicable to this project's scope. |
| No doacross loops | Low | `GOMP_doacross_*` not implemented. |

---

