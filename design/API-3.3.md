# neurogeo 3.3 API

- Explicit time: `ngeo_time_axis()`, `ngeo_validate_time_axis()`,
  `ngeo_time_axis_hash()`, `ngeo_set_time_axis()`,
  `ngeo_get_time_axis()`, and `ngeo_time_slice()`.
- Sparse time relations: `ngeo_temporal_weights()`,
  `ngeo_validate_temporal_weights()`, and `ngeo_temporal_neighbors()`.
- Separable space-time relations: `ngeo_spatiotemporal_weights()`,
  `ngeo_validate_spatiotemporal_weights()`,
  `ngeo_spatiotemporal_lag()`, and the guarded
  `ngeo_materialize_spatiotemporal_weights()`.
- Statistics: `ngeo_temporal_moran()`,
  `ngeo_spatiotemporal_moran()`, `ngeo_temporal_variogram()`, and
  `ngeo_spatiotemporal_variogram()`.
- Longitudinal derivation: `ngeo_longitudinal_change()`,
  `ngeo_temporal_trend()`, and `ngeo_temporal_contrast()`.

The release is additive and retains all earlier exports.
