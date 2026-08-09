# neurogeo 3.2 API

- `ngeo_resampling_plan()` and `ngeo_validate_resampling_plan()` create and
  verify an inert, identity-bound plan.
- `ngeo_build_resampling_map()` explicitly authorizes path application and
  sparse nearest/linear/source-scatter-barycentric/overlap mapping. Surface
  barycentric mapping projects each source vertex to a target triangle and is
  a conservative remap, not target-gather or area-aware Workbench interpolation.
- `ngeo_resampling_diagnostics()` reports coverage, conservation, sparse
  size, issues, uncertainty, and joint path/plan/map identity.
- `ngeo_resample()` produces one target-domain dataset, sparse map, optional
  propagated variance, diagnostics, joint provenance, and optional atomic
  one-artifact output.

No API estimates registration or performs implicit resampling. All earlier
exports remain stable.
