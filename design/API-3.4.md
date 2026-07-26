# neurogeo 3.4 API

- Controls and linear algebra: `ngeo_solver_control()`,
  `ngeo_validate_solver_control()`, `ngeo_iterative_solve()`.
- Approximation: `ngeo_logdet_approx()`.
- Iterative models: `ngeo_spatial_regression_iterative()` and
  `ngeo_car_iterative()`.
- Ordered local-model batching: `ngeo_gwr_batched()` and
  `ngeo_kriging_batched()`.

The release adds no implicit fallback from exact to approximate execution.
