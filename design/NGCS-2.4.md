# Neuroimaging Geoinformatics Core Specification 2.4 addendum

Status: stable  
Version: 2.4  
Base specification: NGCS 2.3

NGCS 2.4 standardizes bounded spatial modelling and prediction while
retaining all NGCS 1.0-2.3 contracts.

- Variogram fitting MUST declare model, weighting, bounds, metric, fitted
  parameters, objective, and convergence.
- Kriging MUST bound local neighbors, return prediction variance, and retain
  ordinary/universal trend and metric identities. It MUST NOT allocate an
  unbounded whole-domain covariance matrix.
- GWR MUST use an eligible explicit metric, finite candidate bandwidths,
  reproducible CV folds, and report local effective sample sizes and
  condition numbers. Singular local designs MUST fail or return declared
  missing results.
- SAR and SEM MUST bind matching sparse weights and record spatial parameter,
  likelihood, log-determinant method, tolerance, and residual
  autocorrelation. Exact dense likelihood MUST have a resource guard.
- Gaussian CAR MUST state proper/intrinsic form, isolates, precision,
  dependence, and identifiability constraint.
- Models compared across support maps MUST bind every operator hash and MUST
  NOT claim invariant coefficients.

Conformance requires variogram parameter recovery, direct small-system
kriging/likelihood checks, finite GWR CV and conditioning, SAR/SEM recovery,
CAR constraints, residual diagnostics, and resource-bound rejection.
