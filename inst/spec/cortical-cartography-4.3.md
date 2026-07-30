# Cortical cartography contract for neurogeo 4.3

## Scope

The module maps the topology and values of an existing `ngeo_surface` to an
auditable two-dimensional chart. It is atlas-independent: no atlas name
selects a hidden geometry, and atlas labels never determine vertex
coordinates.

## Chart kinds

### Parameterization

`method = "imported"` accepts a finite, exactly vertex-aligned two-column
matrix. The caller owns its construction and scientific meaning.

`method = "harmonic"` uses positive uniform edge weights and a convex unit
circle boundary. The mesh MUST be one connected manifold triangulated disk
with Euler characteristic 1, and `boundary` MUST be one ordered cycle equal
to the complete boundary edge set. A closed, disconnected, non-manifold,
multi-boundary, or degenerate source mesh MUST fail before a chart is added.
The implementation MUST NOT invent a cut.

### View projection

Orthographic, PCA, and spherical methods have
`kind = "view_projection"` and `is_metric_flattening = FALSE`.

- orthographic projection records the selected axes;
- PCA records a deterministic two-axis basis;
- spherical projection normalizes coordinates about their centroid, records
  the caller's seam longitude, identifies seam-crossing source edges and
  faces, and renders wrapped copies at both plot boundaries.

These methods make no claim of injectivity, anatomical correspondence,
registration, area preservation, angle preservation, or geodesic fidelity.

## Invariants and provenance

Every chart MUST retain its source domain hash, algorithm, chart kind,
tolerance, exact source vertex IDs, source face indices, topology facts,
boundary or seam where applicable, and a provenance operation.

Distortion is computed per source face and includes source area, signed chart
area, absolute area ratio, fold status, maximum angular error, and mean
angular error. Source triangles MUST have positive finite area. View
projection distortion is diagnostic only.

The anatomical coordinate set, space, faces, element order, support,
measurement semantics, and values block MUST remain unchanged.

## Values and atlases

The map accepts one existing value map or one arbitrary atomic vector with
exactly one item per vertex. Continuous face values are the mean of finite
vertex values. Categorical face values are the modal label; a three-way tie
is resolved deterministically in lexical label order.

An atlas may be an aligned atomic label vector or an `ngeo_partition`.
A partition MUST match either the current domain hash or the exact source
domain hash recorded by the selected chart and MUST have the same element
count. Atlas boundaries are source mesh edges whose endpoint labels differ,
including labelled-to-background edges. Atlas labels affect boundaries only;
they never select or warp geometry.

## Rendering and exchange

Base rendering draws one color per source triangle, optional mesh edges, and
optional atlas boundaries. Spherical seam-crossing triangles and boundary
edges are wrapped and clipped on both sides; they are never drawn across the
full longitude range.

`ngeo_cortical_map_data()` MUST expose enough information to reconstruct the
rendered scientific content: aligned vertices, source faces, face values and
colors, atlas boundaries, legend, palette, limits, missing-value color, chart
metadata, seam metadata, and source mappings. `ngeo_cortical_layout()`
arranges maps only and MUST NOT merge or reinterpret domains.

## Non-claims

The module does not perform surface reconstruction, automatic cutting,
registration, spherical registration, resampling, statistical inference,
quality control, or atlas-template lookup. External neuroimaging software is
not a runtime dependency.

## Release validation

`tools/run-cartography-43-validation.R` emits
`cartography-43-validation.json`. The release gate covers:

- an analytic 1,600-vertex disk with a sparse harmonic solve and zero folded
  faces;
- a 164,025-vertex, 326,432-face semantic exchange workload without any
  dense all-pairs object;
- checksum-pinned left and right HCP 32k GIFTI surfaces bound to their real
  Conte69 32k CIFTI vertex values;
- an explicit aligned eight-sector stress atlas, atlas boundaries, bilateral
  three-panel SVG rendering, source mappings, and spherical seam handling;
- closed-surface, missing-seam, and atlas-misalignment rejection.

The generated stress atlas is not an anatomical reference atlas. Real files
remain download-only under the 4.2.2 fixture governance and are not included
in the package archive.
