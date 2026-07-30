# neurogeo 4.1.1 to 4.2 development plan

## Final objective

Move neurogeo from a broad, tested implementation to an independently
calibrated scientific core without expanding its public model inventory.

Version 4.1.1 closes maintenance and governance inconsistencies. Version
4.2.0 compares existing methods with independent reference implementations,
adds seeded known-parameter calibration, and publishes exact claim
boundaries. Group inference and coverage/QC modules are explicitly deferred.

## Stage 1: 4.1.1 maintenance

- remove the remaining deprecated direct sparse Matrix coercion;
- synchronize maintainer, format, risk, API, migration, and distribution
  metadata;
- require complete unit, conformance, performance, package-check, and
  cross-platform evidence.

Exit: tagged GitHub release with `R CMD check --as-cran` status OK and five
successful R/OS matrix jobs.

## Stage 2: 4.2 reference agreement

- compare Moran, Geary, and local Moran with `spdep`;
- compare SLX, SAR, and SEM with base-model and `spatialreg` references;
- compare spherical variogram and ordinary kriging with `gstat`;
- compare Gaussian GWR coefficients with `GWmodel`;
- record reference versions, seeds, estimands, parameter translations, and
  numeric tolerances.

Exit: every declared common estimand agrees within its registered tolerance.

## Stage 3: 4.2 calibration and edge cases

- estimate null Moran type-I error;
- estimate SAR/SEM parameter bias and RMSE;
- estimate kriging prediction bias and interval coverage;
- estimate GWR coefficient bias and RMSE;
- verify W/B normalization, isolates, missing-value omission, disconnected
  graphs, and seeded reproducibility.

Exit: all predeclared calibration intervals pass without changing thresholds
after observing a failing release result.

## Stage 4: 4.2 release

- publish validation and non-claim documentation;
- pass all existing and new tests, validations, performance gates, package
  checks, and remote CI;
- publish the source archive, manifest, check log, and scientific-validation
  report.

No 4.3 group-inference or 4.4 coverage/QC work is in scope.
