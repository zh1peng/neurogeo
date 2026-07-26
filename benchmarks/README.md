# Benchmarks

Performance checks protect the package's sparse-by-default contract. They are
not micro-optimisation targets.

- `tests/testthat/test-performance.R` constructs a synthetic 32,400-vertex
  surface and checks topology time, sparsity, and memory.
- `benchmarks/run-phase5.R` records topology, weights, Moran, LISA, and
  variogram timings on deterministic synthetic data.

Run from the package root with:

```sh
Rscript benchmarks/run-phase5.R
```

Results are machine-specific and are written to standard output so that a
release log can capture the exact environment.
