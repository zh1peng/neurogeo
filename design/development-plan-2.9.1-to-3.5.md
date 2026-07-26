# neurogeo 2.9.1-3.5 development and release plan

Status: complete  
Baseline: neurogeo 2.9.0 / NGCS 2.9  
Final target: neurogeo 3.5.0

## Overall objective

Evolve the audited 2.9 reference implementation into a stable,
cross-platform, bounded large-project engine while retaining:

- one spatial domain and one strictly aligned values block;
- explicit space, topology, metric, support, measurement semantics,
  transform, time semantics, and provenance;
- sparse, blockwise, deterministic execution with declared resource limits;
- caller-supplied transforms only;
- no FreeSurfer, FSL, or Connectome Workbench runtime dependency;
- no raw MRI preprocessing, automatic registration, implicit resampling,
  full BIDS orchestration, multi-assay container, viewer, tractography, or
  connectome scope.

Every stage must update the specification, API inventory, migration guide,
NEWS, reference documentation, tutorial, validation reports, and release
manifest. A stage is complete only when its unit, property, adversarial,
conformance, golden I/O, integration, simulation, and applicable performance
checks pass and local `R CMD check --as-cran` reports `Status: OK`.

Linux and macOS results count as evidence only when the corresponding remote
job has executed successfully. CI configuration alone remains explicitly
labelled as awaiting remote evidence.

## 2.9.1 - release consistency and installed conformance

### Goal

Remove contradictions between implemented interoperability and shipped
specification material, and make installed-package conformance a
cross-platform CI gate.

### Modules

- synchronized supported-format and output capability documents;
- installed-package NGCS corpus and checksum validation;
- Windows/Linux/macOS plus supported-R CI matrix audit;
- maintenance validation report and release evidence.

### Exit criteria

- source and installed format documents agree with CIFTI/BIDS/schema-2
  implementation;
- an installed package locates and verifies every 2.9 corpus fixture;
- CI declares Windows, Linux, macOS, R release, devel, and oldrel coverage;
- local tests and release check are clean;
- neurogeo 2.9.1 archive, manifest, SHA-256, and check evidence are complete.

## 3.0 - schema, validation, and API lifecycle foundation

### Goal

Publish a stable NGCS 3.0 contract for machine-verifiable object schemas,
portable metadata exchange, classed validation conditions, and API lifecycle
governance without silently removing 2.x behavior.

### Modules

- `ngeo_schema_registry()` and versioned schema descriptors;
- `ngeo_validate_schema()` and structured validation reports;
- portable object metadata manifests with canonical hashes;
- public API lifecycle/status registry and compatibility audit;
- explicit schema migration dispatch and 2.x compatibility layer.

### Exit criteria

- every core object family has a registered schema and invariant set;
- valid objects pass and adversarial mutations return deterministic classed
  issues;
- manifests round-trip independently of R serialization;
- every 2.x export is retained or has an explicit compatibility path;
- neurogeo 3.0.0 release evidence is complete.

## 3.1 - file-backed neuroimaging values

### Goal

Execute bounded analyses directly over supported neuroimaging files without
requiring complete values materialization.

### Modules

- file-backed NIfTI, CIFTI, and MGH/MGZ aligned values;
- frame/map, brain-model, and voxel chunk slicing;
- deterministic chunk iterators integrated with execution plans;
- chunked atomic output, cache identities, checkpoint/resume;
- partial-read and source-file provenance.

### Exit criteria

- file-backed and in-memory results agree for all supported operations;
- selection preserves exact domain and map alignment;
- file mutation invalidates cache/checkpoint identity;
- large dtseries/volume gates remain within declared memory;
- neurogeo 3.1.0 release evidence is complete.

## 3.2 - transform-aware resampling and support bridge

### Goal

Connect supplied transform paths to explicit support-map construction and
authorized resampling without estimating registration.

### Modules

- resampling plan and validation objects;
- nearest, linear/trilinear, barycentric, and overlap map construction;
- coverage, conservation, missing-support, and uncertainty policies;
- transform-path/support-map joint provenance and diagnostics;
- bounded execution and atomic output integration.

### Exit criteria

- identity and direct small references agree exactly or within declared
  tolerance;
- extensive conservation and intensive normalization are verified;
- ambiguity, lossy paths, unsupported interpolation, and incomplete coverage
  fail explicitly;
- no API estimates registration or performs implicit resampling;
- neurogeo 3.2.0 release evidence is complete.

## 3.3 - explicit spatiotemporal semantics

### Goal

Add time-aware analysis while retaining one spatial domain and one aligned
values block.

### Modules

- explicit regular/irregular `ngeo_time_axis`;
- temporal measurement and interval/support semantics;
- temporal and separable spatiotemporal neighbors/weights;
- temporal/spatiotemporal Moran and variogram methods;
- longitudinal change, trend, and support-aware contrast helpers.

### Exit criteria

- no space-by-time geometry expansion is introduced;
- regular and irregular axes are validated and sliced deterministically;
- separable references agree with Kronecker small cases without materializing
  large products;
- temporal support and measurement semantics control valid aggregation;
- neurogeo 3.3.0 release evidence is complete.

## 3.4 - scalable iterative spatial models

### Goal

Provide deterministic, bounded large-domain model fitting with explicit
convergence and approximation evidence.

### Modules

- matrix-free/iterative SAR, SEM, and Gaussian CAR solvers;
- bounded approximate trace/log-determinant with error diagnostics;
- indexed/batched GWR and local kriging;
- solver controls, convergence reports, seeds, and fallback boundaries;
- exact-small versus iterative-large calibration suite.

### Exit criteria

- iterative and exact small references agree within declared tolerance;
- non-convergence and ill-conditioning are classed, visible outcomes;
- worker counts do not change seeded results;
- large-domain gates satisfy time and memory budgets;
- neurogeo 3.4.0 release evidence is complete.

## 3.5 - reproducible workflow and artifact manifests

### Goal

Make complete neurogeo analyses portable, auditable, and replayable without
becoming a workflow orchestrator or multi-assay container.

### Modules

- provenance DAG and validation;
- replay manifest with input, schema, operation, environment, and output
  hashes;
- portable artifact/cache inventory and integrity verification;
- batch derivative manifest with atomic per-artifact transactions;
- lightweight workflow adapters and end-to-end reproducibility report.

### Exit criteria

- DAG cycles, missing parents, mutation, and environment drift are diagnosed;
- a reference workflow replays to identical logical output hashes;
- artifact corruption and incomplete batches fail before use;
- BIDS scope remains explicitly derivative-only;
- neurogeo 3.5.0 archive, final manifest, SHA-256, documentation, and
  `R CMD check --as-cran: Status OK` complete the overall objective.

## Evidence log

- neurogeo 2.9.0 baseline:
  `release/runs/neurogeo-2.9.0-20260726T090018Z`;
  SHA-256
  `bd19bfd4910aa2a575accf1d4900a939319e32e2e6c7cfa6f690db297575e254`;
  892 unit/integration assertions, 33 performance assertions, 16 validation
  reports, and `R CMD check --as-cran: Status OK`.
- neurogeo 2.9.1 completed:
  `release/runs/neurogeo-2.9.1-20260726T100846Z`;
  SHA-256
  `0708cfab755fe8158b243e7ed6cb5f2bc09bf386b9dc5cadc60bc398c5e67bae`;
  902 unit/integration assertions and 33 release-performance assertions
  passed; source/installed format inventories agreed, the installed corpus
  and required specifications verified, the Windows/Linux/macOS CI matrix
  includes the installed-conformance gate without claiming unexecuted remote
  evidence, 17 validation reports were archived, and
  `R CMD check --as-cran` reported `Status: OK`.
- neurogeo 3.0.0 completed:
  `release/runs/neurogeo-3.0.0-20260726T103011Z`;
  SHA-256
  `6d5c489a2888f8c0c41b36cd14d1f15acd7c39f0a60a3c24a46b3efb06e37c8b`;
  930 unit/integration assertions and 33 release-performance assertions
  passed; 18 core schemas were registered, structured adversarial issues
  were deterministic, canonical JSON manifests round-tripped atomically and
  rejected corruption, the NGCS 3.0 corpus verified, all 175 exports were
  stable with no planned removals, 18 validation reports were archived, and
  `R CMD check --as-cran` reported `Status: OK`.
- neurogeo 3.1.0 completed:
  `release/runs/neurogeo-3.1.0-20260726T111357Z`;
  SHA-256
  `eaf200808b96ac5cf56fb56464c4a9147cfc74b69617cc937a4fe57c786c49a5`;
  955 unit/integration assertions and 33 release-performance assertions
  passed; direct file-backed NIfTI, CIFTI, MGH, and MGZ values agreed with
  eager readers, reordered selections preserved domain/map alignment,
  mutation and oversized requests failed explicitly, and complete-source
  copies remained atomic while partial copies were rejected. The bounded
  gates covered 1,000,000 volume elements and 91,592 grayordinates without
  reader-side full values materialization; 19 schemas and 183 stable exports
  were registered, 19 validation reports were archived, and
  `R CMD check --as-cran` reported `Status: OK`.
- neurogeo 3.2.0 completed:
  `release/runs/neurogeo-3.2.0-20260726T115004Z`;
  SHA-256
  `8dd8d321fe4625626d7e50f93d3eb54286ef5f76be9964224b009db75f272e80`;
  1,016 unit/integration assertions and 33 release-performance assertions
  passed. Supplied affine paths reproduced nearest, trilinear, barycentric,
  and overlap references; intensive normalization, extensive conservation,
  value/mapping uncertainty, joint path/plan/map provenance, mutation
  rejection, explicit authorization, and atomic output were verified. The
  100,000-source/100,000-target sparse gate used 100,000 nonzeros, 32.86 MiB,
  and about 1.53 seconds without a dense operator. The release registered 21
  schemas and 188 stable exports, archived 20 validation reports, and
  `R CMD check --as-cran` reported `Status: OK`.
- neurogeo 3.3.0 completed:
  `release/runs/neurogeo-3.3.0-20260726T122600Z`;
  SHA-256
  `93cc64e03386fe5886d21a8c95379cb91e3df3b020e34eb5a1f9aeb2ce9ef46b`;
  1,091 unit/integration assertions and 33 release-performance assertions
  passed. Regular/irregular and instant/interval time axes, mutation-safe
  slicing, temporal semantics, sparse temporal weights, matrix-free
  Kronecker-sum/product lag and Moran references, exact bounded variogram
  pair accounting, and longitudinal helpers were verified. The
  10,000-space by 100-time gate processed 1,000,000 observations while
  storing only 20,002 spatial and 198 temporal nonzeros, without a
  space-time matrix or expanded geometry. The release registered 24 schemas
  and 208 stable exports, archived 21 validation reports, and
  `R CMD check --as-cran` reported `Status: OK`.
- neurogeo 3.4.0 completed:
  `release/runs/neurogeo-3.4.0-20260726T125400Z`;
  archive SHA-256
  `65bfb906854b13fcfec99167c67b7d6c212167f49b17fb5388e859f43ceec2c6`.
  The release passed 1,163 unit and integration assertions plus 33
  performance assertions. Exact small-reference differences were at most
  `5.13e-9` for SAR parameters, `2.90e-8` for SEM parameters, and
  `1.23e-8` for CAR estimates. Seeded log-determinant estimates were
  invariant to worker count, classed non-convergence paths were exercised,
  and batched GWR and kriging stayed within their declared budgets. The
  100,000-element sparse validation gate used 199,998 nonzeros, completed
  CAR in seven iterations and log-determinant estimation with order eight
  and eight probes, performed no dense materialization, and completed in
  0.27 seconds. The release registered 29 schemas and 216 stable exports,
  archived 22 validation reports, and `R CMD check --as-cran` reported
  `Status: OK`.
- neurogeo 3.5.0 completed:
  `release/runs/neurogeo-3.5.0-20260726T132123Z`;
  archive SHA-256
  `6d46590e53a1075310d9b472a7da09b2294b8b12fcf7db4cb861004ec2a00eaa`.
  The complete package passed 1,215 unit and integration assertions plus
  33 release-performance assertions. The reference two-step workflow
  produced a three-node/two-edge provenance DAG and replayed to identical
  logical output hashes. Timestamp invariance, input mutation, environment
  drift, cycles, missing parents, artifact corruption, incomplete state, and
  failed batch publication were exercised as visible gates. A 100,000-element
  logical hash completed in 0.29 seconds within its declared materialization
  and memory budgets without a dense spatial matrix. The release registered
  33 schemas and 233 stable exports, archived all 23 required validation
  reports, and `R CMD check --as-cran` reported `Status: OK`.
- Final roadmap audit completed. The seven 2.9.1-3.5 release manifests,
  source archives, recorded SHA-256 values, and `00check.log` files were
  re-read from disk. Every archive hash matched its manifest and every
  release reported `Status: OK`; see
  `design/release-audit-2.9.1-to-3.5.md`.
