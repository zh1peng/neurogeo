# neurogeo 3.4 development target

## Goal

Add deterministic, resource-bounded iterative spatial model fitting for
domains beyond the exact dense 2.4 boundary. Exact-small methods remain the
normative calibration reference; approximation and convergence are always
visible.

## Modules

1. An immutable `ngeo_solver_control` declaring tolerance, iteration,
   log-determinant trace order/probes, seed, workers, exact-small threshold,
   non-convergence policy, and resource budget.
2. Sparse matrix-vector conjugate-gradient and BiCGSTAB solvers with residual
   histories, classed breakdown/non-convergence outcomes, and no dense
   operator conversion.
3. Deterministic exact-small or Hutchinson power-series
   `log|I - parameter W|` estimation with Monte Carlo standard error,
   truncation diagnostics, admissible spectral bound, and probe batching.
4. Iterative SAR/SEM likelihood fitting and Gaussian proper/intrinsic CAR
   smoothing with explicit solver/log-determinant provenance.
5. Indexed batched GWR and local kriging adapters that preserve target order,
   expose batch diagnostics, and enforce target/materialization budgets.
6. NGCS schemas/corpus, migration/API/specification documents, tutorial,
   exact-small calibration, adversarial tests, deterministic worker checks,
   large sparse gates, and release evidence.

## Exit criteria

- CG/BiCGSTAB solutions and iterative SAR/SEM/CAR fits agree with direct
  small references within declared tolerance.
- Hutchinson estimates are seeded, worker-count invariant, and report
  Monte Carlo plus truncation diagnostics; exact-small mode is explicit.
- Singular/breakdown, non-convergence, inadmissible parameter, mutation, and
  resource overrun are classed visible outcomes.
- Batched GWR/kriging reproduce monolithic target order and values.
- A large sparse model gate does not create a dense weight/operator matrix
  and satisfies declared memory/elapsed limits.
- neurogeo 3.4.0 archive, reports, SHA-256, documentation, and
  `R CMD check --as-cran: Status OK` are complete before 3.5 begins.
