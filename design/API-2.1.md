# neurogeo 2.1 API

All neurogeo 2.0 public functions remain available. Version 2.1 adds these
stable interfaces.

## Builders

- surface: `ngeo_surface_nearest_map()`,
  `ngeo_surface_barycentric_map()`, `ngeo_surface_registration_map()`;
- volume: `ngeo_affine_grid_map()`, `ngeo_voxel_overlap_map()`;
- atlas: `ngeo_atlas_map()`, `ngeo_label_overlap_map()`,
  `ngeo_probabilistic_atlas_map()`.

These consume known relationships and never estimate registration. When no
atlas target is supplied, the returned map stores a constructed regions
template in `map$target`.

## Diagnostics and uncertainty

- `ngeo_support_entropy()`
- `ngeo_support_diagnostics()`
- `plot.ngeo_support_map()`
- `plot.ngeo_support_diagnostics()`
- `ngeo_support_monte_carlo()`
- `ngeo_support_sensitivity()`
- `ngeo_boundary_sensitivity()`

All diagnostic calculations operate on sparse slots or sparse summaries.

## Support-aware inference

- `ngeo_atlas_robust_effect()`
- `ngeo_support_test()`

The former reports a family of target-level support-weighted slopes. The
latter applies one common source permutation across every declared atlas.
Neither function claims local parcellation invariance.

## Sparse exchange

- `write_ngeo_support_map()`
- `read_ngeo_support_map()`

The exchange consists of a Matrix Market operator, a JSON sidecar, and an
optional Matrix Market variance operator.

## Compatibility

The `ngeo_support_map` orientation, measurement-aware change-of-support
rules, domain hash binding, and NGCS 2.0 semantics are unchanged. Validation
in 2.1 additionally rejects corrupt direction, identity, provenance,
tolerance, sparse-object, and derived target-support fields.
