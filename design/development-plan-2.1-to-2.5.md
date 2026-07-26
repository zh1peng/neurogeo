# neurogeo 2.1 to 2.5 final development plan

Status: in progress  
Started: 2026-07-26  
Baseline: neurogeo 2.1.0 / NGCS 2.1

## Final objective

Starting from the validated neurogeo 2.1.0 release, deliver four sequential,
independently validated releases:

```text
2.2 diagnostics and uncertainty
 -> 2.3 support-aware inference
 -> 2.4 spatial modelling and prediction
 -> 2.5 scalable I/O and million-element operations
```

Every release preserves:

```text
one spatial domain
+ one strictly aligned values block
+ explicit space, topology, metric, support, and measurement semantics
+ sparse-by-default computation
+ auditable provenance
```

FreeSurfer, FSL, and Connectome Workbench remain optional external workflow
tools and MUST NOT become runtime dependencies. No release estimates
registration, preprocesses raw MRI, silently resamples, or defaults to a
dense whole-brain all-pairs object.

The final objective is complete only when neurogeo 2.2.0, 2.3.0, 2.4.0, and
2.5.0 each have an append-only source archive, manifest, validation
snapshots, SHA-256, and `R CMD check --as-cran: Status OK`.

## Version discipline

Each version MUST:

1. change only after its API and scientific contracts are implemented;
2. retain all earlier public interfaces or document a deprecation;
3. add NEWS, API, migration, specification, vignette, and risk updates;
4. pass the complete inherited test matrix plus version-specific tests;
5. create an immutable `release/runs/neurogeo-VERSION-TIMESTAMP` directory;
6. finish before development begins on the next version.

Remote pushes, hosted CI, and CRAN/Bioconductor submission require a
separately authorized target and are not local-release exit criteria.

## neurogeo 2.2 — diagnostics and uncertainty

### Modules

- `ngeo_support_condition()`:
  row/column conditioning, effective rank estimates, isolate and weak-support
  diagnostics, bounded sparse spectral calculations.
- `ngeo_support_covariance()`:
  aligned diagonal, sparse, and low-rank covariance contracts.
- `ngeo_support_uncertainty()`:
  analytic and Monte Carlo value/operator propagation, including normalized
  intensive covariance.
- `ngeo_registration_ensemble()` and `ngeo_segmentation_ensemble()`:
  validated ensembles of alternative operators with common ordered domains.
- enhanced `ngeo_support_diagnostics()`:
  coverage by structure, conservation loss, entropy quantiles, effective
  target count, uncertainty summaries, and conditioning.
- enhanced `ngeo_support_sensitivity()`:
  pairwise/reference summaries, uncertainty intervals, and ensemble
  decomposition.

### Scientific boundaries

- covariance assumptions are explicit and dimension-bound;
- normalized uncertainty MUST include numerator/denominator dependence;
- ensembles do not imply that alternatives are independent or calibrated;
- sparse eigensolvers are optional; bounded dense fallbacks have guards;
- categorical uncertainty requires a categorical probability model.

### Exit criteria

- analytic covariance matches Monte Carlo on linear and normalized toy
  cases;
- ensemble hashes and common-domain invariants are enforced;
- diagnostics never densify the full support operator;
- randomized and adversarial covariance/operator tests pass;
- a 100k-source uncertain-operator diagnostic remains bounded;
- neurogeo 2.2.0 archive and check evidence are complete.

## neurogeo 2.3 — support-aware inference

### Modules

- `ngeo_common_support_test()`:
  one shared source-domain permutation/null draw applied across atlases.
- `ngeo_cross_atlas_consensus()`:
  fixed/random-effects consensus with heterogeneity and influence
  diagnostics.
- `ngeo_multiscale_inference()`:
  declared hierarchy of complete support maps, scale-specific estimates,
  multiplicity control, and stability summaries.
- `ngeo_boundary_test()`:
  effect sensitivity to alternative boundary/operator ensembles.
- enhanced `ngeo_atlas_robust_effect()`:
  confidence intervals, heterogeneity, leave-one-atlas-out results, and
  common-source bootstrap.
- `ngeo_support_adjust()`:
  explicit BH/BY/Holm/max-T adjustment for atlas/scale families.

### Scientific boundaries

- a permutation API states whether it preserves topology/autocorrelation;
- consensus is not described as parcellation invariant;
- local, boundary, and multiscale claims identify every tested family;
- max-T uses common simulations and seeded reproducibility;
- underpowered or singular atlas fits fail with classed conditions.

### Exit criteria

- null simulations meet declared family-wise/FDR tolerances;
- known-effect simulations meet bias and interval-coverage tolerances;
- common-source simulations are identical across worker counts;
- cross-atlas consensus matches independent meta-analysis calculations;
- neurogeo 2.3.0 archive and check evidence are complete.

## neurogeo 2.4 — spatial modelling and prediction

### Modules

- `ngeo_fit_variogram()`:
  bounded WLS fitting for nugget, spherical, exponential, and Gaussian
  models.
- `ngeo_kriging()`:
  local sparse ordinary/universal kriging with neighbor limits, prediction
  variance, and support-aware weights.
- `ngeo_gwr()` and `ngeo_gwr_bandwidth()`:
  Euclidean/world/geodesic kernels, leave-one-out or k-fold CV, local
  coefficients, and conditioning diagnostics.
- `ngeo_spatial_regression()`:
  stable OLS, SLX, spatial-lag (SAR), and spatial-error (SEM) adapters using
  matching sparse `ngeo_weights`.
- `ngeo_car()`:
  foundational Gaussian intrinsic/proper CAR estimation with explicit
  constraints; BYM2 remains optional and is implemented only if its scaling
  and identifiability tests are satisfied.
- `ngeo_support_model()`:
  fit/compare declared models across support maps without implicit
  reconstruction.

### Scientific boundaries

- prediction uses bounded local neighborhoods, never an unbounded dense
  covariance matrix;
- GWR requires explicit metric eligibility and reports local condition
  numbers;
- SAR/SEM log-determinant approximations and tolerances are recorded;
- CAR isolates, constraints, and impropriety are explicit;
- model comparison across support does not imply invariant coefficients.

### Exit criteria

- variogram fits recover known parameters within simulation tolerance;
- kriging predictions/variance match independent small references;
- GWR bandwidth CV selects a finite candidate and rejects singular local
  designs;
- SAR/SEM small cases match direct matrix likelihood calculations;
- calibration validates bias, RMSE, coverage, and residual autocorrelation;
- neurogeo 2.4.0 archive and check evidence are complete.

## neurogeo 2.5 — scalable I/O and million-element operations

### Modules

- pure-R `write_ngeo_cifti()` for dscalar, dlabel, and dtseries within the
  supported NGCS brain-model contract, using R backends only;
- `ngeo_delayed_values()` and chunk iteration:
  file-/callback-backed aligned values without changing the one-values-block
  contract;
- `ngeo_block_support_map()`:
  validated row/column block partitioning of one logical sparse
  target-by-source operator;
- blockwise change of support, diagnostics, variance propagation,
  composition, and exchange;
- stable support-map Matrix Market + JSON schema with versioning and
  backwards-compatible reader;
- `ngeo_bids_sidecar()` and `write_ngeo_bids_derivative()`:
  relevant sidecar/spatial entities and derivative provenance only, not
  dataset orchestration;
- million-element performance and resource regression suite.

### Scientific and engineering boundaries

- delayed storage is one logical aligned values block, not a multi-assay
  container;
- block partitioning cannot change element order or operator orientation;
- CIFTI writing preserves brain-model order, surface vertex indices, voxel
  indices, affine, map metadata, labels, and time metadata;
- BIDS support writes derivatives and sidecars but does not index or run a
  BIDS dataset;
- 1M validation remains sparse and avoids full dense materialization.

### Exit criteria

- dscalar/dlabel/dtseries write/read/write golden round-trips pass using no
  Workbench binary;
- delayed and in-memory computations agree within tolerance;
- block and monolithic sparse operators give identical results and hashes
  under the logical-operator contract;
- a 1,000,000-source construction, diagnostic, and change-of-support case
  satisfies recorded time and memory limits;
- BIDS derivative names, spatial entities, JSON provenance, and measurement
  semantics pass fixtures;
- neurogeo 2.5.0 archive, manifest, full documentation, and
  `R CMD check --as-cran: Status OK` complete the final objective.

## Inherited validation matrix

Every version runs:

- unit and API compatibility tests;
- randomized property and adversarial/fuzz tests;
- NGCS 1.0, 2.0, 2.1, and version-specific conformance fixtures;
- NIfTI, GIFTI, CIFTI, and FreeSurfer golden I/O;
- pure-R external and support workflows;
- simulation calibration appropriate to the version;
- 32k/91k/100k/164k inherited performance gates;
- version-specific scale gates;
- vignette and pkgdown builds;
- local `R CMD check --as-cran`.

## Evidence log

- neurogeo 2.1.0 completed:
  `release/runs/neurogeo-2.1.0-20260726T051248Z`;
  SHA-256
  `3fc85ce0ca5c5147bc50cae09b2c96fc2b80bf109e051154ab4d413c26ebbc8b`.
- neurogeo 2.2.0 completed:
  `release/runs/neurogeo-2.2.0-20260726T054011Z`;
  SHA-256
  `2f1dbe6a4e977516fe0ab457aae3d10d6541ba0d6faafbaa7247b9913da5331b`.
- neurogeo 2.3.0 completed:
  `release/runs/neurogeo-2.3.0-20260726T060232Z`;
  SHA-256
  `d75ad898c8a5a951cbab347b06dd12aff50c11dd45d3f2c61791669bfe403326`.
- neurogeo 2.4.0 completed:
  `release/runs/neurogeo-2.4.0-20260726T062644Z`;
  SHA-256
  `f0e9fc09894de2f45b011dfd12a30871be8eab096ccdac96e85545fe89704c4d`.
- neurogeo 2.5.0 completed:
  `release/runs/neurogeo-2.5.0-20260726T064814Z`;
  SHA-256
  `b613103ea62dcc20b42af0cf57d10744dd01af129b04f3917c4e3e4aae46081a`.

The 2.1-2.5 objective is complete. The final 2.5 manifest contains 15
auditable artifacts, all inherited and version-specific validation gates,
and `R CMD check --as-cran: Status OK`.
