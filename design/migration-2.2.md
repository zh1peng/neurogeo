# Migrating from neurogeo 2.1 to 2.2

Version 2.2 is additive.

Independent source variance previously passed to
`ngeo_support_variance()` remains supported. For correlated values or full
target covariance, construct a domain-bound covariance:

```r
covariance <- ngeo_support_covariance(
  source,
  variance = source_variance,
  factor = shared_factor
)
uncertainty <- ngeo_support_uncertainty(
  source,
  target,
  support_map,
  covariance
)
```

Alternative registrations or segmentations should be wrapped in a validated
ensemble before Monte Carlo propagation or sensitivity analysis:

```r
ensemble <- ngeo_registration_ensemble(list(first, second))
```

Quantiles across an ensemble are sensitivity ranges unless the ensemble
weights have a justified probability interpretation.
