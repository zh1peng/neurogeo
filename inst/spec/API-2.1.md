# neurogeo 2.1 API summary

Builders:

- `ngeo_surface_nearest_map()`
- `ngeo_surface_barycentric_map()`
- `ngeo_surface_registration_map()`
- `ngeo_affine_grid_map()`
- `ngeo_voxel_overlap_map()`
- `ngeo_atlas_map()`
- `ngeo_label_overlap_map()`
- `ngeo_probabilistic_atlas_map()`

Diagnostics and sensitivity:

- `ngeo_support_entropy()`
- `ngeo_support_diagnostics()`
- `ngeo_support_monte_carlo()`
- `ngeo_support_sensitivity()`
- `ngeo_boundary_sensitivity()`

Support-aware inference:

- `ngeo_atlas_robust_effect()`
- `ngeo_support_test()`

Sparse exchange:

- `write_ngeo_support_map()`
- `read_ngeo_support_map()`

All neurogeo 2.0 APIs remain available. Builders consume known
registrations or aligned inputs and never estimate registration.
