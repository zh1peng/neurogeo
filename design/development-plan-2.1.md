# neurogeo 2.1 development plan

Status: completed  
Started: 2026-07-26  
Completed: 2026-07-26  
Baseline: neurogeo 2.0.0 / NGCS 2.0

## Product objective

Deliver neurogeo 2.1 as the first release that constructs, audits, and uses
real-world sparse change-of-support operators. The release MUST preserve:

```text
one spatial domain
+ one strictly aligned values block
+ explicit space, topology, metric, and measurement semantics
+ auditable provenance
```

The package will build `target x source` operators from already-known
surface registrations, voxel affines, label segmentations, and
probabilistic atlases. It will quantify coverage, conservation, ambiguity,
operator uncertainty, and result sensitivity before adding a small,
well-defined support-aware inference layer.

The release MUST NOT estimate anatomical registration, preprocess raw MRI,
silently resample, require FreeSurfer/FSL/Connectome Workbench at runtime,
create a custom binary format, or construct an unbounded dense all-pairs
matrix.

## Success criteria

The objective is complete only when all of the following are true:

1. Public constructors return validated sparse `ngeo_support_map` objects
   bound to ordered source and target domain identities.
2. Surface builders handle nearest and barycentric mappings on a declared,
   already-registered coordinate system, respect masks and structure, and
   record distance and fallback decisions.
3. Volume builders handle known affine grids using nearest, trilinear, and
   exact axis-aligned voxel-overlap rules, with explicit partial coverage.
4. Atlas builders accept aligned hard labels and probabilistic memberships,
   create region targets when requested, and preserve label identity.
5. Diagnostics expose source/target coverage, conservation error, effective
   memberships, entropy, ambiguity, sparsity, and operator uncertainty
   without densifying the operator.
6. Sensitivity analysis compares alternative operators on common domains
   and reports value and uncertainty changes. The first support-aware
   inference API reports atlas-robust global estimates and common-support
   permutation tests without claiming local parcellation invariance.
7. Unit, property, adversarial, conformance, golden, integration, and
   performance tests pass. A 100k-source construction/diagnostic workflow
   remains sparse and within declared resource limits.
8. API, specification addendum, migration notes, vignette, NEWS, and
   generated reference documentation describe assumptions and failure
   boundaries.
9. `R CMD check --as-cran` reports `Status: OK` for a reproducible local
   `neurogeo_2.1.0.tar.gz`, and its SHA-256 and validation reports are
   captured in a release manifest.

## Phase 1 — 2.0.1 stability foundation

Scope:

- freeze the NGCS 2.0 support-map orientation and compatibility contract;
- add adversarial validation for malformed sparse slots, IDs, hashes,
  tolerances, uncertainty sparsity, and unsupported semantics;
- add deterministic property tests for crisp/probabilistic/overlapping
  maps, composition, conservation, and hash stability;
- preserve every 2.0 public API and document 2.1 additions.

Exit criteria:

- existing 2.0 tests pass unchanged;
- invalid objects fail with a classed NGCS condition at the public boundary;
- randomized valid operators satisfy their declared invariants;
- no accidental dense conversion occurs in validation.

## Phase 2 — support-map builders

### Surface

- `ngeo_surface_nearest_map()`
- `ngeo_surface_barycentric_map()`
- `ngeo_surface_registration_map()`

The caller supplies source and target surfaces plus named coordinate sets
that already express a common registration space. Nearest mapping is crisp.
Barycentric mapping uses the closest point on target triangles and produces
column-normalized probabilistic weights. Masks, structures, maximum
distance, candidate search, and unmapped policies are explicit.

### Volume

- `ngeo_affine_grid_map()`
- `ngeo_voxel_overlap_map()`
- `ngeo_label_overlap_map()`

Known affine grids support nearest and trilinear mappings. Exact overlap is
limited to axis-aligned voxel grids; unsupported rotations/shears fail
rather than approximate silently. Mask and outside-grid behavior are
explicit.

### Atlas

- `ngeo_atlas_map()`
- `ngeo_probabilistic_atlas_map()`

Hard labels create crisp maps. Probabilities create sparse probabilistic or
overlapping maps according to their column sums. Both require source-row
alignment and stable region IDs.

Exit criteria:

- identity and analytically checkable toy mappings reproduce exact
  operators;
- complete operators have the required column sums;
- partial coverage is never labelled complete;
- every builder records method, coordinate/affine identity, mask policy,
  tolerance, and software version in provenance.

## Phase 3 — diagnostics and uncertainty

Scope:

- `ngeo_support_diagnostics()` and `plot.ngeo_support_diagnostics()`;
- `plot.ngeo_support_map()` for bounded sparse summaries;
- `ngeo_support_entropy()` for normalized membership ambiguity;
- `ngeo_support_monte_carlo()` for sampled operators with fixed sparsity;
- `ngeo_support_sensitivity()` for alternative-map comparisons;
- covariance-aware uncertainty for normalized intensive results through a
  declared Monte Carlo route.

Exit criteria:

- diagnostics do not materialize a dense operator;
- coverage and conservation summaries match direct sparse calculations;
- entropy is zero for crisp assignments and bounded in `[0, 1]`;
- Monte Carlo draws are seeded, non-negative, policy-valid, and auditable;
- sensitivity outputs bind all compared operator hashes and target domains.

## Phase 4 — support-aware inference

Scope:

- `ngeo_support_test()` for common-source permutation comparison;
- `ngeo_atlas_robust_effect()` for estimates repeated across declared
  support maps;
- multiple-testing adjustment for families of declared atlas comparisons;
- explicit boundary-sensitivity summaries.

Exit criteria:

- permutations occur on the common source domain;
- reproducible seeds produce identical results;
- returned effects identify every support operator and never claim general
  local parcellation invariance;
- null simulations meet declared type-I error tolerances.

## Phase 5 — integration and scaling

Scope:

- surface, volume, and hybrid grayordinate workflows using standard-format
  fixtures;
- sparse Matrix Market plus JSON metadata exchange for support maps;
- blockwise construction paths and resource guards;
- release performance case with at least 100k source elements.

Exit criteria:

- workflows require no external neuroimaging executable;
- export/import preserves orientation, IDs, hashes, support, uncertainty,
  and provenance;
- 100k construction, diagnostics, and change-of-support remain sparse,
  conserve extensive values, and satisfy recorded limits.

## Phase 6 — documentation and release

Scope:

- NGCS 2.1 addendum, API 2.1 reference, and migration 2.0-to-2.1;
- a real-world support-mapping vignette and common-mistakes updates;
- package version, NEWS, capability matrix, website, and release checklist;
- conformance, integration, simulation, performance, build, and check
  reports in one immutable release run.

Exit criteria:

- documentation states every algorithmic assumption and unsupported case;
- all validation reports say `passed`;
- local `R CMD check --as-cran` says `Status: OK`;
- the release manifest identifies the source archive and SHA-256.

## Deferred after 2.1

- registration estimation and nonlinear warp estimation;
- arbitrary polyhedral voxel intersection under rotation/shear;
- a general delayed/multi-assay container;
- full CIFTI writing, tractography, connectomes, and interactive viewers;
- variogram model fitting, production sparse kriging, CAR/BYM2, and a full
  SAR/SEM model family;
- unsupported parcel-to-vertex reconstruction presented as exact.

## Evidence log

- Phase 1: all neurogeo 2.0 tests remain passing; stricter identity,
  direction, sparse-slot, tolerance, support-consistency, property, and
  adversarial exchange tests pass.
- Phase 2: surface nearest/barycentric, affine nearest/trilinear,
  axis-aligned voxel overlap, hard-label, and probabilistic-atlas builders
  pass unit and NGCS 2.1 conformance fixtures.
- Phase 3: sparse coverage/conservation/entropy diagnostics, bounded plots,
  fixed-sparsity Monte Carlo uncertainty, operator sensitivity, and
  target-identity assignment sensitivity pass.
- Phase 4: atlas-robust effects and common-source permutation inference
  pass reproducibility tests; 40 null and 20 known-effect scenarios satisfy
  declared calibration limits.
- Phase 5: GIFTI surface, NIfTI segmentation, and CIFTI hybrid atlas
  workflows pass without external binaries. The 100k affine-grid builder
  and diagnostics pass the 30-second/10-MiB sparse gate alongside all
  previous release performance cases.
- Phase 6: API, NGCS addendum, migration, ADR, vignette, reference site,
  NEWS, capability matrix, risks, and release checklist are updated.
- Full unit/property/adversarial/integration suite: passed.
- Final local `R CMD check --as-cran`: `Status: OK`.
- Release archive:
  `release/runs/neurogeo-2.1.0-20260726T051248Z/neurogeo_2.1.0.tar.gz`.
- SHA-256:
  `3fc85ce0ca5c5147bc50cae09b2c96fc2b80bf109e051154ab4d413c26ebbc8b`.
- Public remote creation, hosted CI observation, push, and CRAN/Bioconductor
  submission remain intentionally unperformed pending an authorized target.
