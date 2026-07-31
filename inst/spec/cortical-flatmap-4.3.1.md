# Real cortical flatmap contract for neurogeo 4.3.1

## Goal

Version 4.3.1 renders a real cortical sheet rather than an atlas-specific
cartoon or a generic polygon example. The rendering domain is always the
caller's surface. A registered flat chart, anatomical underlay, vertex map,
mask, and atlas membership remain separate aligned inputs.

## Imported flat-surface binding

`ngeo_flatten_surface(method = "imported", coordinates = flat_surface)` MUST
verify:

1. the source and flat surfaces have the same ordered vertex identifiers;
2. every flat-surface triangle maps to exactly one source triangle,
   independent of winding;
3. no flat triangle is invented or duplicated.

A flat surface MAY contain a strict subset of source faces because an
existing cortical cut or medial-wall treatment can remove triangles. The
chart MUST record `topology_relation = "face_subset"` and retain
`source_face_in_chart`. A coordinate matrix remains supported, but its
topology is necessarily the source topology.

The function does not infer a cut, flatten a closed hemisphere, estimate
registration, or resample values.

## Rendering layers

For a selected chart, `ngeo_cortical_map()` MUST keep one aligned vertex
record and one source-face record. It MAY derive these rendering layers:

- a mask-aware outline from the boundary of complete visible triangles;
- a numeric anatomical underlay such as sulcal depth or curvature;
- a continuous vertex overlay or categorical atlas overlay;
- atlas boundaries along visible mesh edges whose endpoint labels differ;
- one reproducible, vertex-constrained label anchor per visible region.

Face colors are rendering derivatives. Original vertex values, source
indices, space, measurement semantics, and provenance remain unchanged.
Transparent missing colors MUST allow the anatomical underlay to remain
visible.

## Atlas independence

An atlas is an aligned vector, an `ngeo_partition`, or an aligned GIFTI label
map already stored on the source object. Atlas labels never select hidden
geometry. GIFTI label-table names and RGBA colors MAY be reused, but their
membership must still align exactly with the current source vertices.

## Exchange and provenance

`ngeo_cortical_map_data()` MUST expose vertices, faces, inclusion status,
underlay values and colors, atlas boundaries, outline edges, label anchors,
legend, chart metadata, and source mappings. Provenance MUST record the
source-domain hash, included vertex/face counts, atlas source, and chart
source.

## Release gate

`tools/run-flatmap-431-validation.R` MUST pass on checksum-pinned,
download-only HCP S1200 Conte69 32k left and right surfaces, atlas-ROI masks,
one real CIFTI vertex metric, and Schaefer 2018 Conte69 labels. It MUST
produce:

- a bilateral continuous vertex map over an anatomical underlay;
- a bilateral atlas/network map with boundaries, labels, and outlines;
- verified topology mappings, more than 50,000 visible faces per hemisphere,
  and non-empty outlines and atlas boundaries;
- no dependency on FreeSurfer, FSL, or Connectome Workbench binaries.

The fixtures are governed by
`inst/extdata/reference-4.3.1/manifest.csv` and are not bundled in the source
package.
