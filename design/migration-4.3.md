# Migration to neurogeo 4.3

No migration is required for objects or analyses created with 4.2.2.
Version 4.3.0 is additive.

## Replacing atlas-template drawings

Create or import a chart on the actual source surface, then bind aligned
vertex data:

```r
flat <- ngeo_flatten_surface(
  surface,
  method = "harmonic",
  boundary = ordered_boundary
)
map <- ngeo_cortical_map(flat, values = vertex_values, atlas = labels)
plot(map)
```

For a closed cortical surface, do not call harmonic flattening unless the
caller has already supplied an explicit cut that produces a disk. Use an
imported flat chart or a clearly labelled viewing projection:

```r
view <- ngeo_project_surface(surface, "pca")
map <- ngeo_cortical_map(view, values = vertex_values)
```

PCA, orthographic, and spherical outputs are views, not metric-preserving
flattenings or registrations.

## Partition compatibility

A partition created before a chart is added remains usable when its exact
base-domain hash equals the selected chart's recorded source-domain hash.
Unrelated, reordered, resampled, or merely equal-length partitions fail
before rendering.

## Downstream plotting

Use `ngeo_cortical_map_data()` instead of reading undocumented list fields.
The exchange result contains source indices, chart and seam metadata,
face-level values and colors, atlas boundaries, and legend information.
