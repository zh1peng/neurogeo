# neurogeo 4.2.1 to 4.3 development plan

Status: completed through the 4.3.1 real-flatmap correction. Remote CI passed;
the repository no longer publishes per-version GitHub Releases.

## Final objective

Move neurogeo from a scientifically calibrated core to a distributable,
real-data-validated package with a domain-independent cortical cartography
module.

The roadmap has three independently releasable stages:

1. 4.2.1 improves package discovery, examples, citation, API navigation, and
   coverage governance without changing scientific results or object schemas.
2. 4.2.2 validates complete real-file workflows and bounded execution without
   adding a public scientific model.
3. 4.3.0 adds cortical cartography for arbitrary vertex maps and aligned
   atlases. It does not introduce a fixed atlas drawing, implicit
   registration, surface reconstruction, or group inference.

The previously discussed group-inference and coverage/QC modules remain
deferred.

## Stage 1: neurogeo 4.2.1 distribution and usability

### Deliverables

- add the package URL, issue URL, and an installed citation record;
- classify public functions as core, advanced, or exchange/governance APIs;
- add executable examples for the main construction, I/O, support,
  statistics, modelling, validation, and provenance entry points;
- document applicability boundaries and important returned fields in the
  corresponding help pages;
- measure package coverage in CI and prevent regression below a recorded
  release threshold;
- keep the 4.2 numerical estimators, defaults, object classes, and NGCS 3.5
  schemas unchanged.

### Exit criteria

- every declared core entry point has an executable example or is explicitly
  documented as an accessor used by a parent example;
- package coverage is measured reproducibly and does not fall below the
  registered 4.2.1 threshold;
- source, installed help, website, unit tests, release validations,
  performance gates, `R CMD check --as-cran`, and the five R/OS CI jobs pass;
- the tagged release contains the source archive, manifest, check log, and
  coverage report.

## Stage 2: neurogeo 4.2.2 real-data validation

### Workflows

1. NIfTI volume: affine, mask, aligned values, weights/statistics, write,
   reread, and logical identity.
2. GIFTI and FreeSurfer surface: geometry, metric, labels, charts, vertex
   alignment, surface statistics, and format round-trip.
3. CIFTI dscalar, dlabel, and dtseries: ordered brain models, map/time axes,
   partial file-backed reads, bounded summaries, and write/read validation.
4. Atlas and change of support: crisp and probabilistic atlas construction,
   coverage/conservation diagnostics, intensive/extensive semantics,
   uncertainty, and auditable derivative output.

All bundled or downloaded fixtures must have a stable upstream identity,
license record, byte size, and SHA-256. External neuroimaging applications may
be used offline to generate expected results, but remain outside runtime and
CI dependencies.

### Adversarial and scale coverage

- truncated and malformed payloads, reordered axes, invalid datatypes,
  inconsistent label tables, and source mutation;
- disconnected topology, missing vertices, partial atlas coverage, and
  non-uniform support;
- representative 32k/164k surface, 59k/91k grayordinate, and bounded volume
  execution where licensed fixtures permit;
- preservation of element order, coordinate space, measurement semantics,
  logical hashes, and provenance across every supported round-trip.

### Exit criteria

- all four workflows pass and publish a machine-readable report;
- failures are classed and occur before invalid scientific output is exposed;
- large workflows remain sparse or file-backed and satisfy resource gates;
- existing 4.2 scientific calibration remains unchanged;
- complete local and remote release gates pass.

## Stage 3: neurogeo 4.3 cortical cartography

### Scientific boundary

A closed cortical hemisphere cannot be mapped bijectively to one planar chart
without a cut, overlap, or distortion. The API therefore distinguishes:

- an imported or caller-supplied flat chart;
- a disk-topology harmonic/Tutte parameterization with an explicit boundary;
- a spherical-coordinate viewing projection with an explicit seam;
- an orthographic/PCA viewing projection that is never described as a
  flattening or metric chart.

No method estimates anatomical registration or silently cuts a closed mesh.
FreeSurfer, Connectome Workbench, and FSL remain optional external producers,
not runtime dependencies.

### Public module

The minimal public surface is:

- `ngeo_flatten_surface()` to add one audited 2D chart using a supported
  method;
- `ngeo_project_surface()` to add an explicitly non-metric orthographic,
  deterministic PCA, or spherical viewing projection;
- `ngeo_cortical_map()` to bind one chart to vertex data or a matching
  `ngeo_partition`;
- `ngeo_cortical_map_data()` to expose bounded face, vertex, boundary, and
  legend tables for other plotting systems;
- `ngeo_cortical_layout()` to compose named maps or hemispheres without
  merging their scientific domains;
- base-R `plot()` methods for cortical maps and layouts.

The existing `ngeo_set_chart()` and `ngeo_as_sf()` remain compatible.

### Cartography invariants

- every rendered vertex and face retains its source element/face index;
- charts are auxiliary, non-metric coordinate sets and cannot replace
  anatomical geometry, topology, support, or space;
- a harmonic chart requires one connected disk-like triangulation and one
  ordered simple boundary loop;
- a spherical chart records center, seam, longitude/latitude convention, and
  seam-crossing faces;
- viewing projections record the basis and possible overlap and make no
  injectivity claim;
- signed face area, fold count, area scale, angular distortion, boundary,
  seam, domain hash, algorithm, tolerances, and provenance are inspectable;
- partitions must match the current domain hash or the exact source-domain
  hash recorded by the selected chart;
- vertex data must be exactly aligned and no atlas-specific template is
  hard-coded;
- rendering is bounded and never creates an all-pairs distance matrix.

### Validation

- analytic planar and disk meshes with direct harmonic references;
- spherical seam and longitude-wrap adversarial fixtures;
- fold, disconnected, non-manifold, multi-boundary, degenerate-face, and
  domain-mismatch failures;
- arbitrary continuous, categorical, missing, and constant vertex data;
- arbitrary partitions, background labels, region boundaries, palettes, and
  legends;
- golden SVG rendering with canonicalized semantic content plus image
  snapshots for review;
- controlled GIFTI/FreeSurfer flat or spherical geometry and atlas workflows;
- 32k and 164k sparse face-table/performance gates;
- Windows, macOS, Linux, R release/devel/oldrel checks.

### Exit criteria

- disk charts are injective for the conformance corpus and match the direct
  sparse linear-system reference;
- all projection types report their exact claim boundary and distortion;
- vertex and atlas maps render from the same arbitrary source surface without
  atlas-specific geometry;
- exchanged plotting tables reconstruct the rendered faces, values,
  boundaries, and source indices;
- tutorial, API, migration, specification, risk, test, performance, release,
  and remote CI evidence are complete.

## Frozen non-goals

- raw MRI preprocessing or surface reconstruction;
- automatic registration, spherical registration estimation, or arbitrary
  nonlinear resampling;
- a fixed ggseg-style atlas geometry;
- a general interactive viewer or web rendering framework;
- tractography, connectomes, multi-assay containers, or clinical validation;
- group/repeated-measures inference and a new coverage/QC module.
