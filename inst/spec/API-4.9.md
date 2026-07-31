# neurogeo API 4.9

Version 4.9 adds three explicitly experimental facades. They are not part of
the stable 5.0 multilayer inference surface and may remain experimental after
5.0.

## `ngeo_spatial_ordination()`

- delegates MULTISPATI computation to `adespatial`;
- accepts one aligned element-by-layer block and matching spatial weights;
- labels reference-map results as descriptive spatial-map analysis;
- permits frozen projection only after an explicit independent-training
  declaration;
- returns no p-values and never claims population inference.

## `ngeo_coregionalization()`

- fits a bounded LMC through `gstat::fit.lmc()`;
- uses one explicitly seeded uniform sample of unordered element pairs;
- records bin boundaries, NGCS metric, pair population/sample, convention,
  seed, and sample hash;
- requires second-order stationarity and isotropy and verifies every fitted
  sill matrix is positive semidefinite;
- provides shared-scale decomposition only, not co-kriging.

## `ngeo_mgwr()`

- delegates coefficient back-fitting to `GWmodel::gwr.multiscale()`;
- requires fixed user-supplied intercept/predictor bandwidths;
- passes an NGCS distance matrix only after a bounded element-count guard;
- returns effective sample size and condition-number diagnostics;
- suppresses nominal local p-value maps and records unresolved promotion
  blockers.

## Non-goals

4.9 does not add full-cortex pair enumeration, a new core container,
co-kriging, independently implemented MGWR, automatic bandwidth selection,
or confirmatory inference after full-data component selection.
