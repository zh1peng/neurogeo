# neurogeo 2.6-2.9 development and release plan

Status: active  
Baseline: neurogeo 2.5.0 / NGCS 2.5  
Final target: neurogeo 2.9.0 / NGCS 2.9

## Final objective

Deliver four sequential, independently releasable versions that turn the
2.5 reference implementation into a bounded execution and interoperability
platform without changing the core contract:

```text
one spatial domain
+ one strictly aligned values block
+ explicit space, topology, metric, measurement semantics
+ auditable provenance
```

Every version must update the specification, API, migration guide, NEWS,
reference documentation, tutorial, validation reports, and release
manifest. A version is complete only after its unit, property, adversarial,
conformance, golden I/O, integration, simulation, and performance checks
pass and local `R CMD check --as-cran` reports `Status: OK`.

## 2.6 - bounded execution engine

### Modules

- `ngeo_resource_budget()` and resource accounting;
- `ngeo_execution_plan()` with deterministic chunk/block scheduling;
- `ngeo_cache()` keyed by domain, operator, values, semantics, and operation;
- true blockwise change of support, diagnostics, variance, and composition
  without materializing the complete logical operator;
- delayed-native streaming summaries, covariance, spatial statistics, and
  model input preparation;
- atomic output, checksums, checkpoint manifests, and safe resume.

### Boundaries

- delayed storage remains one values block;
- cache identity cannot omit measurement semantics or provenance-relevant
  parameters;
- resource limits fail with classed conditions;
- resume cannot accept a changed domain/operator/values identity;
- no dense whole-domain fallback is implicit.

### Exit criteria

- block and monolithic results/hashes agree across randomized operators;
- delayed and in-memory results agree for every supported streaming method;
- low resource budgets deterministically reject over-budget work;
- interrupted plans resume without duplicated or missing blocks;
- atomic-write failure leaves no partial final output;
- one-million-source execution remains sparse and within recorded limits;
- neurogeo 2.6.0 archive, manifest, SHA-256, and check evidence are complete.

## 2.7 - uncertainty-aware spatial modelling

### Modules

- uncertainty-aware variogram estimates and fitted parameter simulations;
- kriging with measurement/operator/support uncertainty decomposition;
- GWR coefficient covariance, local intervals, and bandwidth sensitivity;
- SAR/SEM parameter and prediction simulation using matching support
  covariance;
- CAR posterior/MAP uncertainty under declared Gaussian assumptions;
- cross-support model ensembles with within/between support variance;
- calibration reports for bias, RMSE, interval coverage, and residual
  autocorrelation.

### Boundaries

- uncertainty is domain-bound and dimension-checked;
- analytic formulas state linearization and independence assumptions;
- simulation draws share source/operator realizations where required;
- sensitivity ranges are not confidence intervals without a probability
  model;
- no Bayesian claim is made by deterministic CAR smoothing.

### Exit criteria

- analytic small cases match direct matrix references;
- Monte Carlo propagation agrees within declared tolerance;
- known-effect simulations meet bias and coverage thresholds;
- seeded results are identical across supported worker counts;
- neurogeo 2.7.0 release evidence is complete.

## 2.8 - explicit space and transform graph

### Modules

- `ngeo_space_registry()` with stable identities and aliases;
- `ngeo_transform_graph()` with explicit directed transform edges;
- deterministic path search, inversion eligibility, and composition;
- structure, units, dimensionality, template, density, and resolution audit;
- graph and path diagnostics plus provenance export;
- explicit application helper that requires caller authorization and never
  estimates a transform.

### Boundaries

- matching names are not proof of spatial equivalence;
- the graph stores supplied transforms only;
- no registration, resampling, interpolation, or repair is automatic;
- ambiguous paths fail unless the caller selects one;
- lossy or non-invertible edges cannot be silently inverted.

### Exit criteria

- path composition matches direct affine references;
- cycles, aliases, ambiguity, unit/structure mismatch, and mutation are
  rejected or explicitly diagnosed;
- provenance binds every traversed edge and its hash;
- neurogeo 2.8.0 release evidence is complete.

## 2.9 - interoperability and 3.0 readiness

### Modules

- extended CIFTI NamedMap metadata, label tables, time axes, datatype and
  brain-model corpus validation;
- BIDS derivative entity parser/builder, naming, sidecar validation, atomic
  writing, and collision policy;
- NGCS support-map exchange schema 2 with atomic bundles, checksums, chunk
  metadata, schema-1 reader compatibility, and migration;
- language-independent JSON fixtures and reference results for NGCS
  1.0-2.9;
- cross-platform compatibility matrix and API/deprecation inventory for 3.0.

### Boundaries

- no external neuroimaging runtime binary;
- CIFTI axes outside the supported contract fail explicitly;
- BIDS remains derivative I/O, not dataset orchestration;
- schema 2 does not create a custom neuroimaging binary format;
- 3.0 readiness does not remove a 2.x API.

### Exit criteria

- dscalar/dlabel/dtseries corpus round-trips preserve supported metadata;
- BIDS names/entities/sidecars pass valid and adversarial fixtures;
- schema 1 and 2 readers reproduce the same logical operator and hash;
- language-independent conformance corpus is self-describing and versioned;
- public API and planned deprecations are audited;
- neurogeo 2.9.0 archive, final manifest, SHA-256, full documentation, and
  `R CMD check --as-cran: Status OK` complete the objective.

## Evidence log

- neurogeo 2.5.0 baseline:
  `release/runs/neurogeo-2.5.0-20260726T064814Z`;
  SHA-256
  `b613103ea62dcc20b42af0cf57d10744dd01af129b04f3917c4e3e4aae46081a`.
- neurogeo 2.6.0 completed:
  `release/runs/neurogeo-2.6.0-20260726T073550Z`;
  SHA-256
  `7b2db74cd923bae2e04f89ea8dabddd1aa6a03a76bcddc8bedbe887aebdf499b`;
  798 unit/integration assertions passed, 33 release-performance assertions
  passed, the one-million-source bounded gate completed in 3.92 seconds with
  zero value error and no logical-operator materialization, and
  `R CMD check --as-cran` reported `Status: OK`.
- neurogeo 2.7.0 completed:
  `release/runs/neurogeo-2.7.0-20260726T080517Z`;
  SHA-256
  `f39ab628b027f29436a99ce5f54d750f0fca3b0a455476b83f6687f241d6d888`;
  826 unit/integration assertions and 33 release-performance assertions
  passed; direct kriging/CAR references had zero analytic error, 5000-draw
  kriging Monte Carlo relative error was 0.022, known-effect coverage was
  0.935, seeded 1/2-worker SAR simulations were identical, and
  `R CMD check --as-cran` reported `Status: OK`.
- neurogeo 2.8.0 completed:
  `release/runs/neurogeo-2.8.0-20260726T082540Z`;
  SHA-256
  `e3d0caf05c9ab6f4875117d742b48c7da650a315d68214f5204f27dcc8180db5`;
  853 unit/integration assertions and 33 release-performance assertions
  passed; affine composition/application direct-reference errors were zero,
  cycle/ambiguity/mismatch/lossy/mutation adversarial gates passed, and
  `R CMD check --as-cran` reported `Status: OK`.
- neurogeo 2.9.0 completed:
  `release/runs/neurogeo-2.9.0-20260726T090018Z`;
  SHA-256
  `bd19bfd4910aa2a575accf1d4900a939319e32e2e6c7cfa6f690db297575e254`;
  892 unit/integration assertions and 33 release-performance assertions
  passed; CIFTI scalar/label/series metadata and datatype gates had zero
  reference error, all BIDS valid/adversarial/atomic gates passed, schema-1
  and schema-2 operators and hashes agreed, checksum mutation was rejected,
  all 166 public exports were inventoried with zero 2.x deprecations, and
  `R CMD check --as-cran` reported `Status: OK`.
