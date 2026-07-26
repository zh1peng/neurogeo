# neurogeo 3.2 API

## Planning and validation

- `ngeo_resampling_plan()` binds exact domains, one selected transform path,
  interpolation and policy choices, uncertainty declarations, and resource
  limits without performing resampling.
- `ngeo_validate_resampling_plan()` rejects changed domains, paths, policies,
  parameters, budgets, or plan identity.

## Mapping and diagnostics

- `ngeo_build_resampling_map()` requires explicit authorization, applies only
  a non-lossy affine path, and delegates to the existing nearest, trilinear,
  barycentric, or overlap builder.
- `ngeo_resampling_diagnostics()` reports sparse coverage, conservation,
  support totals, uncertainty declaration, structured issues, and joint
  path/plan/map identity.

## Execution

- `ngeo_resample()` requires explicit authorization and returns an
  `ngeo_resampling_result` containing the target-domain dataset, sparse map,
  optional propagated variance, diagnostics, provenance, and optional atomic
  one-artifact output metadata.

The result dataset still has one spatial domain and one aligned values block.
The API does not estimate registration, choose transform paths, hide lossy
operations, or create a general workflow/container abstraction.

All neurogeo 2.x, 3.0, and 3.1 exports remain available and stable.
