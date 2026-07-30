# neurogeo 4.2.1 public API tiers

## Purpose

The tiers make a broad public namespace easier to navigate. They describe
the expected user journey; every exported function remains supported under
the existing lifecycle policy.

## Stable core

### Domains and aligned values

- constructors: `ngeo_surface()`, `ngeo_volume()`, `ngeo_points()`,
  `ngeo_grayordinates()`, `ngeo_regions()`;
- access: `ngeo_domain()`, `ngeo_elements()`, `ngeo_values()`, `ngeo_maps()`,
  `ngeo_measures()`, `ngeo_labels()`, `ngeo_subset()`;
- semantics and identity: `ngeo_space()`, `ngeo_measure()`,
  `ngeo_domain_hash()`, `ngeo_validate()`.

### Geometry, topology, and weights

- `ngeo_adjacency()`, `ngeo_distance()`, `ngeo_neighbors()`,
  `ngeo_weights()`, `ngeo_components()`;
- `ngeo_vertex_area()`, `ngeo_voxel_volume()`, `ngeo_support_size()`;
- `ngeo_partition()`, `ngeo_boundary()`, `ngeo_region_adjacency()`,
  `ngeo_aggregate()`.

### Support and spatial analysis

- `ngeo_atlas_map()`, `ngeo_probabilistic_atlas_map()`,
  `ngeo_support_map()`, `ngeo_support_diagnostics()`,
  `ngeo_change_support()`;
- `ngeo_moran()`, `ngeo_geary()`, `ngeo_local_moran()`,
  `ngeo_variogram()`;
- `ngeo_fit_variogram()`, `ngeo_kriging()`, `ngeo_gwr()`,
  `ngeo_spatial_lm()`, `ngeo_spatial_regression()`.

### Input, output, and reproducibility

- `read_ngeo()` and the NIfTI, GIFTI, CIFTI, and FreeSurfer readers;
- `write_ngeo()` and the corresponding format writers;
- `ngeo_example_data()`, `ngeo_object_manifest()`, `ngeo_logical_hash()`,
  and provenance access/export.

### Cortical cartography (added in 4.3)

- chart construction: `ngeo_flatten_surface()` and
  `ngeo_project_surface()`;
- vertex/atlas maps: `ngeo_cortical_map()` and
  `ngeo_cortical_map_data()`;
- independent panel composition: `ngeo_cortical_layout()`.

These are stable core entry points subject to the explicit topology,
projection, seam, distortion, and non-registration contract in
`cortical-cartography-4.3.md`.

## Advanced scientific API

This tier contains null models, common-support inference, uncertainty,
iterative models, resampling plans, time/space-time analysis, model
ensembles, transform graphs, streaming computation, and bounded
file-backed values. Users must follow each function's explicit
parameterization, approximation, support, and claim boundaries.

## Exchange and governance API

This tier contains format-contract validators, support bundles and schema
migration, BIDS derivative helpers, portable manifests, replay, artifact
batches, conformance helpers, registry/audit operations, and optional
`sf`/`spdep`/`igraph` conversion.

These functions exist to make boundaries auditable. They do not define a
second scientific object model.
