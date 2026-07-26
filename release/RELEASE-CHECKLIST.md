# neurogeo 3.5.1 local release checklist

## Specification and API

- [x] NGCS 1.0 through NGCS 3.5 specifications are frozen in `design/`.
- [x] Five domain classes and the one-domain/one-values-block invariant remain
  stable.
- [x] Space, topology, metric, measurement semantics, transforms, provenance,
  file-backed execution, resampling, spatiotemporal analysis, iterative models,
  and reproducible replay have explicit contracts.
- [x] API compatibility, migration, deprecation, governance, and contribution
  policies are documented.

## Scientific correctness

- [x] Distance-based weights execute the declared coordinate, hop, or
  edge-geodesic metric.
- [x] Kriging and GWR use the requested domain metric; kriging variance uses
  the corrected constrained-system sign.
- [x] SAR parameter optimization respects the admissible spectral interval.
- [x] Temporal maps reject incompatible measurements and derived maps propagate
  rate, percent, trend, and integral units explicitly.
- [x] Local Moran declares conditional or total randomization, and conditional
  expectations match `spdep`.
- [x] Variogram custom bins must cover every retained pair.
- [x] File-backed identity includes source, selection, dimensions, and
  verification policy; manifests use canonical nested object ordering.
- [x] Reproducible artifact batches require and verify `artifacts.json`.
- [x] Dense all-pairs operations remain guarded by explicit execution limits.

## Formats, workflows, and documentation

- [x] NIfTI, GIFTI, CIFTI, and FreeSurfer golden I/O passes without external
  neuroimaging binaries.
- [x] Surface, volume, grayordinate, partition, support-map, resampling,
  spatiotemporal, spatial-model, and replay conformance validations pass.
- [x] Twenty vignettes build and rebuild during package checking.
- [x] Supported formats, glossary, capability matrix, common mistakes, and
  distribution route are documented.

## Engineering

- [x] Windows R 4.4.3 `R CMD check --as-cran --no-manual` reports `Status: OK`.
- [x] The full `testthat` suite passes.
- [x] All 23 required release validation reports pass.
- [x] Large performance gates pass for a 164,025-vertex surface, 91,592
  grayordinates, 100k KNN/support operations, and bounded large workloads.
- [x] A local package repository makes dependency-cycle checks deterministic
  and network-independent.
- [x] The source archive, checksums, check log, validation snapshots, and
  session information are captured in an immutable release run.
- [x] Linux/macOS/Windows CI jobs are defined.
- [ ] Linux and macOS CI results are observed on an authorized remote.

## Publication

- [ ] Public repository and release maintainers are designated.
- [ ] CRAN submission is explicitly authorized.

The unchecked remote-CI and publication items do not invalidate the local
3.5.1 release candidate. They remain gates for public distribution.
