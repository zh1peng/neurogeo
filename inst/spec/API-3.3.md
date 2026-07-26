# neurogeo 3.3 API

- `ngeo_time_axis()`, `ngeo_validate_time_axis()`, and
  `ngeo_time_axis_hash()` construct and verify explicit regular/irregular,
  instantaneous/interval time axes.
- `ngeo_set_time_axis()`, `ngeo_get_time_axis()`, and `ngeo_time_slice()`
  bind map-aligned temporal semantics and slice time without rebuilding the
  spatial domain.
- `ngeo_temporal_weights()` and `ngeo_temporal_neighbors()` construct sparse
  adjacent, index-lag, or time-distance relations.
- `ngeo_spatiotemporal_weights()` retains separable spatial and temporal
  operators. `ngeo_spatiotemporal_lag()` is matrix-free;
  `ngeo_materialize_spatiotemporal_weights()` is a guarded small-reference
  helper.
- `ngeo_temporal_moran()`, `ngeo_spatiotemporal_moran()`,
  `ngeo_temporal_variogram()`, and `ngeo_spatiotemporal_variogram()` provide
  identity-bound, resource-bounded exploratory statistics.
- `ngeo_longitudinal_change()`, `ngeo_temporal_trend()`, and
  `ngeo_temporal_contrast()` create spatially aligned derived maps with
  support-aware temporal interpretation.

No API infers a time axis from map count, duplicates geometry through time,
or estimates registration/resampling. All earlier exports remain stable.
