# neurogeo 1.0.1–2.0 development plan

Status: completed  
Started: 2026-07-26  
Completed: 2026-07-26  
Baseline: neurogeo 1.0.0 / NGCS 1.0

## Product objective

Evolve the stable one-domain/one-values-block core into a locally
release-ready neurogeo 2.0. The path deliberately separates usability and
engineering work in 1.x from the NGCS 2.0 change-of-support research
contract.

The following remain out of scope: raw MRI preprocessing, registration
estimation, implicit resampling, tractography/connectome containers, a full
viewer, a custom binary format, and default dense all-pairs operations.

## Milestone 1 — neurogeo 1.0.1

Status: completed

Scope:

- classed importer failures for malformed or truncated inputs;
- adversarial alignment, index, header, mask, and metadata tests;
- reproducible release/conformance/performance scripts;
- release, devel, and oldrel R CI preparation on Linux, macOS, and Windows;
- clean-checkout release documentation.

Exit criteria:

- all malformed-input tests fail at the reader boundary with a classed
  `ngeo_error_io` or more specific NGCS condition;
- no importer invokes FreeSurfer, FSL, or Workbench;
- unit, golden I/O, conformance, external workflow, and Windows
  `R CMD check --as-cran` pass;
- source archive construction is repeatable without silently deleting an
  earlier release record.

## Milestone 2 — neurogeo 1.1

Status: completed

Scope:

- controlled 2D computational charts and one-way `sf` export;
- diagnostic plotting for five domains, weights, and partitions;
- Getis–Ord Gi/Gi*, spatial correlograms, unified permutation control, and
  multiple-testing adjustment.

Exit criteria:

- chart geometry never replaces anatomical geometry, support, topology, or
  metric;
- exported `sf` objects retain domain hash and distortion metadata;
- Gi/Gi* and correlograms match independent references;
- 32k surface diagnostics remain bounded and do not create one complex
  geometry object per vertex by default.

## Milestone 3 — neurogeo 1.2

Status: completed

Scope:

- composition, inversion, validation, and application of known transforms;
- standard-format writers and read/write/read golden round-trips;
- bounded scalable KNN, distance-band, and radius queries;
- auditable and redactable provenance export.

Exit criteria:

- transforms require matching source/target spaces and never estimate
  registration;
- round-trips preserve domain mapping, affine, map order, labels, masks, and
  measurement metadata within format capability;
- no writer depends on external neuroimaging binaries;
- neighbor construction remains sparse and has explicit resource guards.

## Milestone 4 — neurogeo 1.3

Status: completed

Scope:

- surface spin and graph/Moran-constrained null models;
- foundational spatial regression adapters;
- geodesic kernel regression with explicit bandwidth and support;
- simulation-based calibration and reproducible parallel RNG.

Exit criteria:

- every null/model states its domain, topology, metric, support, and
  coordinate requirements;
- simulations validate type-I error, bias, or coverage against declared
  tolerances;
- hemispheres, medial wall, isolates, and missing data have explicit
  policies;
- mismatched domain/weights/transform objects are rejected before fitting.

## Milestone 5 — neurogeo 2.0 / NGCS 2.0

Status: completed

Scope:

- frozen `ngeo_support_map` contract;
- sparse crisp, probabilistic, and overlapping support operators;
- support-aware aggregation and uncertainty propagation;
- cross-atlas transfer, overlap, and comparison;
- parcellation-invariant inference;
- NGCS 1.x migration and compatibility helpers.

Exit criteria:

- extensive quantities are conserved under valid support changes;
- intensive quantities use declared support-normalized operators;
- operator composition and domain hashes are validated;
- no ill-posed parcel-to-element reconstruction is presented without a
  model, regularization, and uncertainty;
- language-independent NGCS 2.0 fixtures and at least three atlas/simulation
  scenarios pass;
- local 2.0 source archive, checksums, documentation, paper update, and
  release checks are reproducible.

## Versioning discipline

Each milestone gets its own NEWS section, migration note, tests, and local
release evidence. Public repository creation, pushes, CI execution on a
remote, CRAN/Bioconductor submission, and publication require an explicitly
designated remote or release target.

## Completion evidence

- Final package: neurogeo 2.0.0 / NGCS 2.0.
- Local source archive:
  `release/runs/neurogeo-2.0.0-20260726T042755Z/neurogeo_2.0.0.tar.gz`.
- SHA-256:
  `0f4e7dd9b35a5dc2b73b85a6eca4908e8b9ccbb56148d678b57f030f487f8597`.
- Local `R CMD check --as-cran`: `Status: OK`.
- NGCS 1.0 conformance, NGCS 2.0 support-map conformance, external
  workflows, simulation calibration, and release performance: passed.
- Public remote creation, push, hosted CI observation, and registry
  submission remain intentionally unperformed pending an authorized target.
