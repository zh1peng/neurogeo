# Resource-budget contract in 6.0

Status: accepted for the 6.0 audit candidate.

All four defaults are deliberately `Inf`, meaning ordinary calls are
unlimited. A caller that requires bounded execution must pass finite limits.
`memory_bytes` and `materialized_elements` are conservative pre-allocation
estimates; `blocks` counts scheduled chunks; `elapsed_seconds` is checked from
the start of an accepting operation at each implemented block boundary.

The streaming file reader, change-of-support loop, and group-permutation
orchestration establish deadline contexts and check them before and after
bounded blocks. A deadline raises `ngeo_error_resource_deadline` with code,
field, and recovery hint. Parallel clusters and atomic temporary outputs retain
their existing `on.exit` cleanup. The deadline is cooperative: it prevents the
next block from starting and cannot interrupt a single non-cooperative backend
call already in progress.
