# Migrating from neurogeo 2.3 to 2.4

Version 2.4 is additive. `ngeo_variogram()`, `ngeo_spatial_lm()`, and
`ngeo_kernel_regression()` remain available.

Use `ngeo_fit_variogram()` before bounded local `ngeo_kriging()`. Use
`ngeo_gwr_bandwidth()` to select only from explicit candidates and pass its
result to `ngeo_gwr()`. Use `ngeo_spatial_regression(model = "sar")` or
`model = "sem"` only with matching weights; the reference likelihood is
exact and intentionally size-guarded. `ngeo_support_model()` compares the
supplied supports and does not establish invariant coefficients.
