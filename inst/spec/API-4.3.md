# neurogeo API 4.3

Version 4.3.0 adds an atlas-independent cortical cartography layer. It does
not change existing NGCS objects, estimators, file readers, or measurement
semantics.

## Public functions

- `ngeo_flatten_surface()` adds an imported two-dimensional parameterization
  or computes a uniform-weight harmonic parameterization for a connected
  triangulated disk with an exact ordered boundary loop.
- `ngeo_project_surface()` adds an orthographic, deterministic PCA, or
  longitude/latitude viewing projection. These charts are explicitly
  non-metric. Spherical viewing requires an explicit seam longitude.
- `ngeo_cortical_map()` binds one surface chart to arbitrary aligned vertex
  values and, optionally, an aligned crisp atlas or `ngeo_partition`.
- `ngeo_cortical_map_data()` returns plotting-system-neutral vertex, face,
  atlas-boundary, legend, chart, distortion, and provenance data.
- `ngeo_cortical_layout()` arranges independent maps or hemispheres without
  merging their domains.

`print()` and base-R `plot()` methods are registered for
`ngeo_cortical_map` and `ngeo_cortical_layout`.

## Object boundary

A chart remains an auxiliary two-dimensional coordinate set on an
`ngeo_surface`. It never replaces the active anatomical geometry, faces,
space, support, metric, values block, or measurement metadata.

An `ngeo_cortical_map` is a bounded rendering/exchange object, not a second
scientific dataset container. It retains:

- the source domain hash and chart-source domain hash;
- every source vertex ID and face index;
- chart method, kind, topology invariants, seam, and distortion;
- face-level values and colors;
- optional atlas boundary edges and legend data.

## Compatibility

All 4.2.2 functions, arguments, return fields, object classes, numerical
defaults, and NGCS 3.5 schemas remain supported. Existing caller-supplied
charts made with `ngeo_set_chart()` continue to work.

The module does not estimate anatomical or spherical registration, cut a
closed mesh, resample vertex data, infer atlas alignment, or provide a fixed
atlas drawing.
