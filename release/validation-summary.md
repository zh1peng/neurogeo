# neurogeo 3.5.1 local release validation

Generated: 2026-07-26 (Asia/Shanghai)

## Result

The scientific audit findings were corrected and the package now builds and
installs as `neurogeo` 3.5.1 while retaining the NGCS 3.5 public contract.

- Windows R 4.4.3 `R CMD check --as-cran --no-manual`: **Status OK**.
  Installation, dependency, source, namespace, code, documentation, tests,
  vignettes, and vignette rebuilding all passed. CRAN incoming and system-clock
  network checks were disabled locally.
- Full source test suite: **passed**. New regression coverage includes metric
  execution, kriging variance, GWR distance semantics, SAR spectral bounds,
  temporal unit transformations, Local Moran null models, variogram bin
  coverage, file identity, canonical manifests, and artifact-index validation.
- Release validations: **23 of 23 passed**, covering NGCS conformance,
  simulations, golden/external I/O, support maps and inference, spatial models,
  scalable and bounded execution, uncertainty, space graphs, interoperability,
  schema, file-backed I/O, resampling, spatiotemporal analysis, iterative
  models, and reproducible replay.
- External RNifti workflow: **passed**, with 114,555 active voxels and 667,534
  sparse nonzero weights.
- External CIFTI workflow: **passed**, with 59,412 grayordinates and 355,488
  sparse nonzero weights.
- Release performance: **passed** for the 164,025-vertex surface, 91,592
  grayordinates, 100k KNN/support workloads, and declared bounded-execution
  gates.
- Twenty vignettes: **built and rebuilt successfully** during package checking.

## Corrected scientific issues

Version 3.5.1 removes mismatches between declared and executed metrics in
weights, kriging, and GWR; corrects constrained kriging variance; bounds SAR
optimization by the weight spectrum; enforces temporal measurement
compatibility and derived units; makes Local Moran null models explicit; and
prevents incomplete variogram bins from silently dropping pairs.

It also strengthens reproducibility by binding file-backed identities to their
actual source and selection, canonicalizing nested manifest objects, requiring
artifact indexes, and declaring the direct `xml2` dependency used by CIFTI
metadata parsing.

## Release artifact

`release/runs/neurogeo-3.5.1-20260726T152053Z/neurogeo_3.5.1.tar.gz`

- Size: 386,168 bytes
- MD5: `9fdffb007da1869239284d592ca94b72`
- SHA-256:
  `40249690544a7266e53004adab0b769460cfb26944355d74dd447a2adc82baad`

The immutable run also contains `manifest.json`, the `Status: OK` check log,
session information, and all 23 validation snapshots. `release/LATEST` points
to this run.

## External gates

Linux and macOS jobs are configured in `.github/workflows/R-CMD-check.yaml`,
but cannot be observed until an authorized remote repository is designated.
Public repository operations, remote CI, and CRAN submission were not
performed.
