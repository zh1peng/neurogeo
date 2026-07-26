# Migrating from neurogeo 1.1 to 1.2

neurogeo 1.2 preserves the 1.1 object and analysis contracts.

- `ngeo_apply_transform()` applies only a supplied, validated affine and
  changes geometry only. It never estimates registration or resamples
  values.
- Writer functions return manifests. NIfTI manifests include a mask path;
  pass that path back to `read_ngeo_nifti()` to preserve active-domain
  membership.
- GIFTI writers return named geometry, metric, and label paths because the
  format stores these in separate files.
- Coordinate weights use exact search up to
  `options(neurogeo.max_exact_neighbors)` and the optional `dbscan`
  KD-tree backend beyond it.
- `ngeo_export_provenance()` can redact paths or all source identifiers and
  checksums before JSON export.
