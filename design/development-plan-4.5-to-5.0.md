# neurogeo 4.5 to 5.0 execution plan

Status: 4.5-4.9 complete; 5.0 freeze and validation in progress

Authoritative detailed plan: `../neurogeo_4.5-5.0_development_plan_zh.md`.
Scientific-method issue: <https://github.com/zh1peng/neurogeo/issues/6>.

## Objective

Deliver one auditable vertical workflow for support-aware multiscale layer
coupling with subject-level inference:

1. exact map-column indexing by independent unit and layer;
2. a fixed, value-independent, topology-derived spatial basis;
3. support-weighted projection and scale-specific layer endpoints;
4. whole-subject covariate-adjusted permutation inference;
5. one common permutation schedule over a declared support family;
6. calibrated simulation, real-data, resource, documentation, and package
   evidence for version 5.0.

## Stages

- 4.5: layer contract, exact map binding, graph basis, and projection.
- 4.6: same-location, spectral, and directional layer coupling.
- 4.7: exchangeability, Freedman-Lane, max-T, and group inference.
- 4.8: common-schedule inference across declared supports.
- 4.9: optional experimental ordination, bounded cross-variograms, LMC, and
  MGWR feasibility. These methods do not block 5.0.
- 5.0: freeze APIs and specifications; finish calibration, real-data,
  performance, documentation, and distribution evidence.

The detailed Chinese plan controls mathematical definitions, task IDs, exit
criteria, promotion rules, resource gates, and the final Definition of Done.

## Completion evidence

### 4.5.0

- Exact layer indexing and map binding tests pass for memory, delayed, and
  verified file-backed values.
- Analytic path, cycle, grid, disconnected-component, support-weighted
  orthogonality, reconstruction, and degeneracy tests pass.
- The installed package, complete unit suite, examples, and the two new
  Chinese vignettes pass.
- The 32,400- and 91,200-element validation gates pass with 64 modes, sparse
  partial eigensystems, no dense full-domain matrix, and maximum residual
  below `3.2e-9`.
- Tarball checking passes package installation, code, documentation,
  examples, and tests. Full legacy-vignette execution still exposes four
  pre-existing tutorial examples that depend on absent files or obsolete
  calls; this remains a documented 5.0 distribution blocker rather than a
  4.5 scientific claim.

### 4.6.0

- One API returns same-location, band structure, spectral coupling,
  directional lag, and classic cross-Moran endpoints in the existing compact
  `ngeo_subject_features` representation.
- Support replication, area imbalance, missing unit-layers, constant maps,
  intensive measurement semantics, energy-only changes, coupling-only
  changes, eigenvector signs, and complete-degenerate-band rotations pass.
- Classic bivariate Moran matches `spdep::moran_bv()` exactly in the reference
  case; direction and weight normalization remain endpoint identity.
- Reference-map nulls require explicit randomized/fixed stacks and a shared
  transformation group, reject invalid joint targets, and record a group hash
  plus `population_inference = FALSE`.
- The 32,400- and 91,200-element by six delayed-map gates pass with 64 modes,
  two-map projection chunks, finite endpoints, and no dense full-domain
  matrix. Cotangent and general neighborhood features remain unexported.

### 4.7.0

- Free, within-block, sign-flip, and user schedules pass identity, duplicate,
  alignment, restriction, exact-enumeration, hash, and resource tests.
- The Freedman--Lane engine uses shared QR decompositions, complete subject
  records, endpoint/permutation blocks, streamed exceedance/maxima, and no
  endpoint null matrix by default.
- Explicit fixed-schedule calculations and `permuco::lmperm()` match exactly;
  one and two workers preserve schedule order and numerical results.
- Full 100-replicate calibration gives FWER 0.06 (pure null), 0.06 (nuisance),
  and 0.04 (site blocks), plus sign-flip type-I 0.05; all are below the
  predeclared binomial 99.5% upper gate of 0.11.
- Sparse max-T and distributed sum-of-squares power are 0.98 and 0.95 in the
  declared simulations. A 200-subject by 256-endpoint by 999-transformation
  run streams without retaining endpoint nulls or permuting vertices.
- The installed end-to-end Chinese vignette, full unit suite, package
  examples, and tarball core checks pass.

### 4.8.0

- A named list extends the existing group facade without adding a public API
  or support-family container. Endpoint columns are bound before one common
  schedule and one full-family max-T calculation.
- Exact unit order, support names/hashes, analysis order, family hash, and
  complete-family subjects are enforced across every declared support.
- Semantic keys gate direction/dispersion/persistence/leave-one-out summaries;
  rank-matched, physical, and unmatched scales remain explicitly distinct.
- Existing boundary diagnostics are descriptive only. No boundary p-value is
  reused as subject inference, no support variance is added to sampling
  variance, and no stable/parcellation-invariant label is generated.
- In 100 correlated-support null replicates, endpoint type-I is 0.0369 and
  full-family FWER is 0.01. The 160-subject, 10-support, 640-endpoint,
  999-transformation gate completes with one schedule hash, no endpoint null
  retention, and no automatic support-uncertainty combination.
- The Chinese support-family vignette, full unit suite, package examples, and
  tarball core checks pass.

### 4.9.0

- `ngeo_spatial_ordination()` delegates to `adespatial::multispati()`, labels
  reference-map work as non-population inference, and blocks frozen projection
  unless independent training is declared.
- One uniformly sampled and hashed pair schedule drives all auto/cross
  empirical variograms; complete-pair values match `gstat` exactly.
- `ngeo_coregionalization()` delegates LMC fitting to `gstat`, records
  stationarity/isotropy, checks every sill matrix for PSD, and exposes no
  co-kriging.
- `ngeo_mgwr()` passes a bounded NGCS distance matrix and fixed term-specific
  bandwidths to `GWmodel`, returns effective-N/conditioning diagnostics, and
  exposes no nominal local p-value map.
- All three APIs remain experimental. Dense MGWR distances are limited to
  small domains and explicitly prevent full-cortex use; unresolved spatial
  null, bandwidth-uncertainty, and support-replication gates are retained.

## Workflow policy

Development is committed directly to `main`. No pull request, version tag, or
GitHub Release is created. Each scientific behavior is specified and tested
before its public facade is added. Existing unrelated worktree changes are
preserved.
