# Migrating from neurogeo 1.2 to 1.3

Version 1.3 adds methods without changing the 1.2 object or writer contracts.

- Surface spin requires an explicitly named registration coordinate set;
  anatomical or visualization coordinates are never treated as spheres.
- Moran spectral nulls are deliberately bounded and are not a default
  whole-brain dense operation.
- `ngeo_spatial_lm(model = "slx")` means spatial lags of predictors. It is
  not a SAR lag or spatial-error estimator.
- `ngeo_kernel_regression()` requires an explicit bandwidth and reports the
  exact metric, kernel truncation, support policy, and target domain hash.
- Fixed per-replicate seeds make results identical across supported serial
  and PSOCK worker counts.
