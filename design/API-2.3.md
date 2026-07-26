# neurogeo 2.3 API

## Common-support testing

- `ngeo_common_support_test()`
- `ngeo_support_adjust()`

One source-domain permutation, Moran spectral realization, or surface spin is
reused across every atlas. Results state whether the selected null preserves
spatial autocorrelation. BH, BY, Holm, and single-step max-T adjustments are
available.

## Consensus and multiscale inference

- `ngeo_cross_atlas_consensus()`
- `ngeo_multiscale_inference()`

Consensus provides fixed or DerSimonian-Laird random effects, heterogeneity,
confidence intervals, and leave-one-atlas-out influence. Multiscale inference
retains the caller-declared scale order and reports adjacent changes and
stability.

## Boundary inference

- `ngeo_boundary_test()`
- enhanced `ngeo_boundary_sensitivity()`

Boundary tests consume a validated segmentation ensemble with one ordered
source and target domain.

## Enhanced atlas effects

`ngeo_atlas_robust_effect()` now adds atlas-specific confidence intervals,
random-effects consensus, leave-one-atlas-out estimates, and an optional
seeded common-source paired-value bootstrap.

All neurogeo 2.2 APIs remain available.
