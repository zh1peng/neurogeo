# Migrating from neurogeo 2.2 to 2.3

Version 2.3 is additive. Existing `ngeo_support_test()` calls retain their
unconstrained common-source permutation behavior.

For an explicitly named null family and max-T correction, use:

```r
test <- ngeo_common_support_test(
  source,
  support_maps,
  targets,
  outcome = "outcome",
  predictor = "predictor",
  null = "permutation",
  adjustment = "maxT",
  seed = 23
)
```

Use `null = "moran"` with matching `ngeo_weights`, or `null = "spin"` with
valid spherical registration coordinates, when the scientific null must
preserve spatial structure.

Atlas-specific effects now include confidence intervals and a random-effects
consensus:

```r
effect <- ngeo_atlas_robust_effect(
  source,
  support_maps,
  targets,
  outcome = "outcome",
  predictor = "predictor",
  bootstrap = 999,
  seed = 23
)
```

Consensus and multiscale summaries describe only the declared support family.
They are not parcellation- or scale-invariance claims.
