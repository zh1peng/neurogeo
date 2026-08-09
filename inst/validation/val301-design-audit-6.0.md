# VAL-301 design audit and required amendment

Status: blocked before primary result generation. The original Phase 3 hash
remains immutable; this audit does not alter it and is not calibration evidence.

The frozen `SIM-null-factorial` grid names autocorrelation, trend, geometry,
missingness, and `conditional`/`total` levels, but it does not identify all of
the quantities required for a type-I experiment:

- the public null procedure and exact test statistic;
- the null-generating distribution for each procedure;
- whether non-zero spatial autocorrelation is part of the null or an
  alternative whose rejection measures power;
- how `conditional` and `total` randomization apply outside local Moran tests;
- endpoint definitions for the registered within-map multiplicity family;
- a matched comparator for each geometry and missingness mechanism.

Treating a spatially autocorrelated field as a replicate from a spatial
randomness null would label legitimate power as type-I error. Applying local
Moran conditional/total semantics to surface spin or the eigen-sign surrogate
would test a different null. Either choice would be an unregistered analysis
decision.

Before VAL-301 can run, a separately hashed amendment must name, for every
cell, the procedure, statistic, generative null, tested hypothesis,
multiplicity family, comparator, and expected failure behavior. It must also
separate null calibration from power cells and freeze any mask/trend fitting
performed before randomization.

Current safe runtime boundary: `ngeo_spin_null()` and `ngeo_moran_null()` are
experimental lifecycle entries, reject their default calls, require explicit
`experimental = TRUE`, report `status = "experimental_uncalibrated"`, and do
not claim to preserve spatial autocorrelation. This containment is checked by
`tools/check-val301-safe-boundary-60.R`; it does not satisfy C02.
