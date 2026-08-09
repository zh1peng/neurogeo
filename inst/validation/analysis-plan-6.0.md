# Frozen analysis plan for central 6.0 claims

Status: preregistered internally before Phase 3 result generation. This is not
an external registration, independent validation, or publication result.

## Common decision rules

- Nominal two-sided alpha is 0.05. Calibration cells use 5,000 independent
  replicates unless an exact enumeration is smaller. At probability 0.05 this
  gives Monte Carlo SE about 0.0031; every report includes a Wilson 95% interval.
- A null-calibration cell passes when the upper Wilson bound is at most 0.065.
  Coverage cells pass only when the Wilson interval is compatible with the
  preregistered 0.93–0.97 range; point estimates alone are insufficient.
- Equivalence bounds, reference tolerances, failed-fit rates, multiplicity
  families, missingness mechanisms, geometry cells, and random seeds are
  parameters in an immutable design artifact, not values chosen after plots.
- Every attempted replicate counts. Failed fits are reported by cell and count
  against a maximum 1% failure rate unless a method-specific lower limit is
  frozen before execution. A run stops for corrupted input, invalid reference
  identity, or a numerical failure rate above 5%; it does not stop for a
  favorable result.
- Factorial null cells cover positive/negative spatial autocorrelation, trend,
  irregular sampling, missingness, mesh hemisphere/medial-wall structure, and
  the registered sampling unit. Resampling cells separate gather interpolation
  from conservative remapping. Operator cells compare empirical weighted,
  Dirichlet, logistic-normal, and independent-Gaussian ablations.
- Primary gates are evaluated once on the frozen design. Debug runs use
  different IDs and cannot replace registered replicates. Negative results
  trigger the stop rule in the matrix.

## Evidence boundaries

Internal tests and simulations may establish software behavior but not external
validity. Real-data claims require a discovery dataset and a cohort/site that
is independent under predefined rules, with at least one completely public
workflow. At least one external validator must rerun the key simulations from
the immutable release artifact before any central scientific claim is marked
complete.
