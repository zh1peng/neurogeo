# neurogeo 1.3 method boundaries

## Surface spin

`ngeo_spin_null()` requires a surface, a 3D coordinate set explicitly marked
`registration`, approximately spherical radii, finite values, and an
installed R `dbscan` backend. Optional hemisphere/structure strata rotate
independently and mappings cannot cross strata. It does not estimate a
spherical registration. Nearest-neighbor spin mappings may repeat vertices.

## Moran spectral null

`ngeo_moran_null()` requires matching sparse weights and numeric values. It
uses a symmetric graph operator and random eigen-coefficient signs, preserving
centered variance and the Moran quadratic form to a declared numerical
tolerance of `1e-6`. The exact eigendecomposition has an explicit default
limit of 2,000 observations; it is not a default dense whole-brain method.
Missing values and isolates follow explicit `na_action` and `zero_policy`
arguments.

## Foundational spatial regression

`ngeo_spatial_lm()` fits OLS or spatial-lag-of-X (SLX) models. It does not
present OLS/SLX as a SAR lag or spatial-error estimator. Every response,
predictor, result row, and optional weights object is bound to one domain
hash. Residual Moran's I is diagnostic, not a replacement for model
assumptions.

## Spatial kernel regression

`ngeo_kernel_regression()` fits local weighted least squares with an explicit
metric, bandwidth, kernel, truncation, target set, missing-data policy, and
optional domain-support weights. Surface defaults to edge geodesic distance.
It does not estimate bandwidth automatically and does not extrapolate a
singular local design unless the caller explicitly accepts `NA` results.

## Reproducibility

Simulation methods pre-generate one seed per replicate. Serial and PSOCK
worker execution therefore return identical replicate streams for the same
seed. Release validation declares tolerances for permutation type-I error,
coefficient bias, kernel exact-field recovery, and Moran/variance
preservation.
