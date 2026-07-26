# neurogeo 2.2 API

## Covariance and propagation

- `ngeo_support_covariance()`
- `ngeo_validate_support_covariance()`
- `ngeo_support_uncertainty()`

Covariance can be diagonal, matrix, or low-rank plus diagonal. Analytic
output may be diagonal-only or a bounded full covariance. Monte Carlo output
may incorporate one validated operator ensemble.

## Ensembles

- `ngeo_support_ensemble()`
- `ngeo_registration_ensemble()`
- `ngeo_segmentation_ensemble()`
- `ngeo_validate_support_ensemble()`

Every ensemble binds identical ordered source and target domains and carries
an integrity hash.

## Conditioning and diagnostics

- `ngeo_support_condition()`
- enhanced `ngeo_support_diagnostics()`
- enhanced `ngeo_support_sensitivity()`

All neurogeo 2.1 APIs remain available.
