# neurogeo 3.4 API

- `ngeo_solver_control()` and `ngeo_validate_solver_control()` define
  immutable convergence, approximation, seed, worker, and resource policy.
- `ngeo_iterative_solve()` provides sparse/matrix-free CG and BiCGSTAB with
  complete convergence reports and classed failure policy.
- `ngeo_logdet_approx()` selects guarded exact-small evaluation or seeded
  Hutchinson power series with Monte Carlo and truncation diagnostics.
- `ngeo_spatial_regression_iterative()` fits bounded SAR/SEM likelihoods with
  explicit optimization, log-determinant, and solve evidence.
- `ngeo_car_iterative()` fits a declared-precision proper or intrinsic
  Gaussian CAR smoother through sparse CG.
- `ngeo_gwr_batched()` and `ngeo_kriging_batched()` process indexed targets
  in deterministic resource-bounded batches without changing target order.

Existing exact-small modelling APIs remain unchanged and serve as calibration
references. All earlier exports remain stable.
