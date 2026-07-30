# Scientific validation contract for neurogeo 4.2

## Scope

The 4.2 gate validates common, explicitly matched estimands:

| neurogeo method | Independent reference | Matched quantity |
|---|---|---|
| `ngeo_moran()` | `spdep::moran()` | global Moran's I |
| `ngeo_geary()` | `spdep::geary()` | global Geary's C |
| `ngeo_local_moran()` | `spdep::localmoran()` | local Moran Ii with population variance |
| `ngeo_spatial_lm(model = "slx")` | base weighted-design OLS | intercept, X, and lag-X coefficients |
| `ngeo_spatial_regression()` | `spatialreg` | SAR/SEM parameter, coefficients, and log likelihood |
| spherical variogram and `ngeo_kriging()` | `gstat` | semivariance, ordinary prediction, and prediction variance |
| `ngeo_gwr()` | `GWmodel::gwr.basic()` | local Gaussian-kernel coefficients |

Reference comparison uses small deterministic point grids, row-standardized
or binary sparse weights, a Euclidean metric, and common model conventions.
Absolute tolerances are `1e-10` for descriptive statistics and `1e-6` for
fitted-model quantities unless a stricter check is recorded.

## Parameterization translations

The spherical model uses the same practical-range parameterization as
`gstat`. neurogeo exponential and Gaussian variograms use a practical-range
form:

```text
exponential: 1 - exp(-3 h / range)
gaussian:    1 - exp(-3 (h / range)^2)
```

Comparisons with software using scale parameters must therefore use
`scale = range / 3` and `scale = range / sqrt(3)`, respectively. The 4.2
kriging reference uses the directly matched spherical form.

neurogeo Gaussian GWR uses `exp(-0.5 (distance / bandwidth)^2)` and truncates
at three bandwidths. The reference fixture keeps every pair within that
cutoff, so truncation does not change the matched estimand.

## Calibration gates

All simulations use fixed seeds recorded in the release report.

- Moran null rejection at alpha 0.05 must lie in `[0.01, 0.12]` over 80
  independent fields with 199 permutations each.
- Across 50 known-parameter fields, absolute SAR/SEM parameter bias must be
  below `0.12` and RMSE below `0.25`.
- Across 120 Gaussian fields, ordinary-kriging prediction bias must be below
  `0.20` in absolute value and nominal 95% interval coverage must lie in
  `[0.88, 0.99]`.
- Across 60 constant-coefficient fields, absolute mean GWR coefficient bias
  must be below `0.05` and coefficient RMSE below `0.15`.

These intervals are release regression gates, not universal guarantees.

## Edge cases

The gate separately verifies:

- binary and row-standardized weights;
- explicit isolate rejection and `zero_policy = TRUE`;
- explicit missing-value omission and reconstruction of the declared
  normalization on the retained raw-weight subgraph;
- disconnected but non-isolated graphs;
- identical seeded permutation output.

## Claims not supported

The 4.2 evidence does not establish:

- clinical validity or suitability for diagnosis;
- correctness after unknown registration or implicit resampling;
- asymptotic guarantees for every domain, topology, or sampling design;
- Bayesian interpretation of the deterministic CAR smoother;
- equivalence outside matched variogram and kernel parameterizations;
- large-domain validity of exact dense SAR/SEM likelihoods;
- family-wise error control beyond the method and null model explicitly used;
- group-level or repeated-measures neuroimaging inference.

The existing resource guards, measurement semantics, space checks, and
provenance requirements remain part of every supported claim.
