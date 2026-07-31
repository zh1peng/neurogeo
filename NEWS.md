# neurogeo 4.5.0

* Added exact `unit x layer` map indexing with duplicate, completeness, and
  measurement-consistency diagnostics without materializing aligned values.
* Added exact ordered-domain map-column binding for in-memory, delayed, and
  verified file-backed values. Binding never registers or resamples inputs
  and preserves source identities and mutation checks.
* Added fixed support-weighted graph-Laplacian bases with component-separated
  sparse partial eigensystems, residual/nullity/orthogonality diagnostics,
  immutable hashes, and explicit resource gates.
* Added chunked basis projection to one compact independent-unit by endpoint
  matrix. Coefficients, band energy, retained variance, residual energy, and
  roughness share one support-weighted geometry.
* Validated path, cycle, and grid spectra, complete small-case reconstruction,
  delayed/in-memory equality, and 32,400/91,200-element 64-mode workflows
  without a dense full-domain matrix. This version does not yet claim layer
  coupling or subject-level inference.

# neurogeo 4.4.2

* Consolidated installed specifications under `inst/spec/` and documented
  `design/` as the location for plans, audits, risks, and frozen historical
  rationale. Current contracts no longer require a mirrored design copy.
* Removed unused archive/Release builders, tracked historical check logs, and
  duplicate website copies of cortical figures generated from vignette
  sources.
* Renamed the complete local validation and performance entry points to
  maintenance-focused names. This version adds no public function, class, or
  object schema.

# neurogeo 4.4.1

* Added a shared exact-model dimension guard before CAR smoothing and CAR
  posterior uncertainty can construct dense matrices. Oversized requests fail
  with `ngeo_error_resource`.
* Extended `write_ngeo()` auto-dispatch to standard CIFTI dscalar, dlabel, and
  dtseries suffixes while retaining the pure-R writer and dedicated API.
* Replaced an internal `xml2` namespace lookup with the package's public API
  and added direct regression coverage for previously indirect public
  functions.

# neurogeo 4.4.0

* Added one bounded `ngeo_qc()` entry point for scientific conditions that do
  not necessarily invalidate an NGCS object. It reports space and measurement
  metadata, bounded value summaries, sparse topology, cortical-chart
  distortion, optional support coverage/conservation, and provenance without
  mutating inputs.
* Added machine-readable QC status records plus compact print and plot methods.
  Large values blocks and topology checks are explicitly marked as not
  evaluated when they exceed their declared budgets.
* Kept `ngeo_validate()` authoritative for structural invariants. QC does not
  repair objects, infer semantics, register spaces, or add a new object schema.

# neurogeo 4.3.2

* Added optional shared continuous scales and categorical color contracts to
  bilateral and multi-panel cortical layouts. Shared layouts draw one legend
  while preserving each panel's source domain, face mapping, mask, underlay,
  and provenance.
* Conflicting categorical label colors, mixed continuous/categorical panels,
  and differing continuous palettes now fail before a misleading shared
  legend can be rendered.
* Added focused semantic and portable-rendering tests without adding a
  plotting dependency or atlas-specific geometry.

# neurogeo 4.3.1

* Replaced the illustrative toy-map workflow with a real cortical-sheet
  workflow built from an imported, registered flat surface. Imported GIFTI
  surfaces may retain a verified subset of source faces while preserving
  exact source-vertex identity and a source-face mapping.
* Added mask-aware cortical outlines, anatomical underlays, transparent
  continuous or categorical overlays, atlas label-table colors, region
  boundaries, label anchors, and aligned atlas lookup by label-map name.
* Added checksum-pinned, download-only HCP S1200 Conte69 flatmap and
  Schaefer 2018 validation fixtures. Bilateral 32k validation now produces
  real continuous-vertex and atlas/network maps without FreeSurfer,
  Connectome Workbench, or other external neuroimaging binaries.
* Reworked the Chinese and English cortical-cartography tutorials around the
  real bilateral flatmap workflow and made the scientific boundary explicit:
  neurogeo verifies an existing chart; it does not infer a cortical cut,
  registration, or resampling.

# neurogeo 4.3.0

* Added atlas-independent cortical cartography with
  `ngeo_flatten_surface()`, `ngeo_project_surface()`,
  `ngeo_cortical_map()`, `ngeo_cortical_map_data()`, and
  `ngeo_cortical_layout()`.
* Added imported and harmonic disk parameterizations plus explicitly
  non-metric orthographic, PCA, and spherical viewing projections.
  Closed surfaces are never cut automatically, and spherical views require
  an explicit seam.
* Added source vertex/face identity, topology invariants, seam-crossing
  metadata, per-face area/angular distortion, chart/domain hashes, and
  provenance to cortical map exchange data.
* Added arbitrary aligned continuous or categorical vertex maps, aligned
  vector or `ngeo_partition` atlases, atlas boundaries, base-R rendering,
  hemisphere/multi-map layouts, semantic golden fixtures, real HCP 32k
  validation, and a Chinese tutorial.

# neurogeo 4.2.2

- Added an immutable, license-separated external fixture manifest and verified
  download cache for real Conte69 CIFTI, HCP 32k GIFTI, and FreeSurfer MGZ
  validation data; external data remain outside the source archive.
- Added four machine-readable real-data workflows covering NIfTI,
  GIFTI/FreeSurfer, CIFTI dscalar/dlabel/dtseries, and atlas/change-of-support
  behavior with sparse/file-backed resource gates and adversarial failures.
- Recorded which representative scales are exercised and explicitly report
  164k/91k scale cases as not exercised when no fixture with compatible terms
  is available. No external neuroimaging application is a runtime or CI
  dependency.
- Corrected validation/build library precedence so a project-local install is
  loaded before an older system installation when both are present.
- Corrected the unit-test runner to execute source tests through the package
  loading environment instead of silently succeeding when an ordinary
  installation contains no installed test files.

# neurogeo 4.2.1

* Added package, documentation-site, issue-tracker, and installed citation
  metadata for standard discovery and attribution.
* Classified the public API into stable core, advanced scientific, and
  exchange/governance tiers without changing any exported function.
* Added executable examples and clearer scientific boundaries to the main
  construction, spatial relationship, support, statistics, modelling, I/O,
  validation, and provenance entry points.
* Added reproducible line-coverage reporting and a non-regression CI gate.
  Scientific estimators, defaults, object schemas, and NGCS 3.5 semantics are
  unchanged.
* Corrected transform manifest generation to hash the canonical
  `source_space` and `target_space` fields; valid transforms can now produce
  portable audit manifests.

# neurogeo 4.2.0

* Added an auditable scientific-validation gate for global and local spatial
  association, SLX/SAR/SEM, variogram/ordinary kriging, and Gaussian GWR.
  Common estimands are compared against `spdep`, `spatialreg`, `gstat`, and
  `GWmodel` with explicit cross-platform tolerances.
* Added seeded known-parameter simulations for Moran type-I error, SAR/SEM
  parameter bias and RMSE, kriging prediction calibration, and GWR coefficient
  bias. Edge cases cover weight normalization, isolates, missing values,
  disconnected graphs, and reproducible permutations.
* Corrected `na_action = "omit"` spatial statistics to rebuild the declared
  W/B normalization on the retained raw-weight subgraph. Row-standardized
  analyses with missing elements can therefore differ from 4.1 results.
* Documented validated claims, parameterization translations, resource
  boundaries, and claims that the evidence does not support. No public
  function, class, model inventory, or NGCS 3.5 schema is added.

# neurogeo 4.1.1

* Removed the final direct `dgCMatrix` coercion from spatial-model weight
  subsetting and added a regression test for warning-free sparse conversion.
* Updated package-maintainer metadata and synchronized the installed format
  inventory, risk register, and distribution-route status with the 4.1
  release line. Disambiguated the Chinese support-inference vignette index
  title so package checks no longer report a duplicate-title note.
* Added maintenance API and migration records. Scientific objects, numerical
  methods, and NGCS 3.5 semantics are unchanged.

# neurogeo 4.1.0

* Added six byte-pinned, license-audited upstream NIfTI, GIFTI, CIFTI, and
  FreeSurfer reference fixtures plus `ngeo_example_data()` for discovery and
  integrity verification.
* Added reference-format tests and release validation for read/write
  round-trips, affine and brain-model indexing, metadata, source provenance,
  truncated files, malformed topology, missing transforms, and alignment
  failures.
* Added an executable Chinese format workflow tutorial with volume, surface,
  and grayordinate visualizations. All workflows continue to use pure-R
  backends without FreeSurfer, FSL, or Connectome Workbench binaries.
* Completed the Matrix sparse-coercion update across weights, resampling,
  models, statistics, and support operations, removing deprecated direct
  coercions while preserving sparse numerical semantics.

# neurogeo 4.0.1

* Added a fully executable Chinese getting-started tutorial covering object
  construction, validation, sparse weights, Moran's I, change of support,
  file readers, bounded values, manifests, common errors, and review checks.
* Updated pkgdown navigation to the 4.0 public API and added installation,
  contribution, issue-reporting, and documentation-site infrastructure.
* Replaced deprecated Matrix pattern-to-numeric sparse coercion in topology
  construction without changing adjacency or weighting semantics.

# neurogeo 4.0.0

* Reduced the public API to scientific domain, support, model, I/O,
  validation, and replay contracts. Removed the 3.6 compatibility shims for
  metric objects, delayed-value construction, block-support wrappers,
  generic execution/cache utilities, separate batched model wrappers, and
  schema/conformance introspection.
* `ngeo_change_support()` now has one sparse support-map execution path with
  explicit resource budgets. Internal delayed and atomic-write machinery is
  no longer presented as a second scientific object or public workflow API.
* `ngeo_validate()` is the single public validator for registered NGCS
  objects. Portable object manifests remain available without exposing a
  mutable schema registry or attribute-only migration layer.
* Consolidated version-suffixed source files into responsibility-named
  modules and removed obsolete print-only result classes.
* The stable NGCS 3.5 scientific schemas and numerical semantics are
  unchanged; 4.0 is an API and implementation architecture release.

# neurogeo 3.6.0

* Introduced the 4.0 lifecycle transition: generic execution/cache helpers,
  block-support wrappers, metric objects, schema introspection, and separate
  batched-model wrappers are now declared deprecated.
* `ngeo_change_support()` now accepts legacy block support maps directly, and
  `ngeo_validate()` validates every registered NGCS object through one public
  entry point.
* Added a migration guide that separates retained scientific contracts from
  implementation-only frameworks scheduled for removal in 4.0.

# neurogeo 3.5.2

* Binary `ngeo_delayed_values` selections now seek directly to requested
  cells and validate backing-file size instead of loading the complete file.
* Metric objects reject parameters that have no execution semantics.
* Execution-plan checkpoints and content-cache keys now bind the executor or
  compute implementation as well as the declared scientific inputs.
* Replay operations and release/conformance version metadata now have one
  data-driven definition instead of repeated version and operation ladders.

# neurogeo 3.5.1

* Fixed distance-based weights, local kriging, and GWR bandwidth selection
  to execute their declared metric, including surface edge-geodesic
  distances, and to reject unsupported metric requests.
* Corrected covariance-form kriging variance and constrained exact SAR/SEM
  likelihoods to the weights-specific spectral parameter interval.
* Enforced compatible measurement types, spatial semantics, and units across
  time maps; derived percent, rate, trend, and integral maps now carry
  transformed units and conventional signed percent change.
* Strengthened file-backed identities, recursively canonicalized manifest
  object keys, declared Local Moran randomization nulls, required complete
  variogram bin coverage, and verified the standalone artifact manifest in
  derivative batches.
* Added adversarial regression tests for folded surfaces, kriging algebra,
  binary-weight SAR bounds, temporal unit safety, file identity collisions,
  canonical property order, local inference nulls, and artifact completeness.

# neurogeo 3.5.0

* Added budgeted scientific logical hashes that include ordered domains,
  aligned values, semantics, labels, and file identities without depending
  on incidental provenance timestamps.
* Added immutable acyclic provenance DAGs and environment-bound,
  whitelist-only workflow recording/replay with verified input,
  intermediate, and output hashes.
* Added portable root-relative artifact manifests and atomically published
  derivative-only batches with corruption, incompleteness, and failed-writer
  detection before use.
* Added NGCS 3.5 schemas, language-independent corpus, migration/API/spec
  documents, tutorial, adversarial tests, and a 100,000-element logical-hash
  release gate while retaining every earlier API.

# neurogeo 3.4.0

* Added immutable solver controls and sparse/matrix-free CG and BiCGSTAB
  solutions with residual histories, classed breakdown/non-convergence, and
  hard resource limits.
* Added guarded exact-small and seeded Hutchinson power-series spatial log
  determinants with worker-invariant estimates, Monte Carlo standard errors,
  and conservative truncation diagnostics.
* Added iterative SAR/SEM likelihood fitting and declared-precision proper or
  intrinsic Gaussian CAR smoothing with explicit convergence and
  approximation provenance.
* Added ordered, resource-bounded indexed GWR and local-kriging batches that
  reproduce monolithic target results.
* Added NGCS 3.4 schemas, language-independent corpus, migration/API/spec
  documents, tutorial, exact-small calibration, adversarial tests, and a
  100,000-element sparse release gate while retaining earlier APIs.

# neurogeo 3.3.0

* Added immutable regular/irregular, instant/interval time axes with exact
  map binding, temporal measurement semantics, deterministic slicing, and
  mutation detection while preserving one spatial domain.
* Added sparse temporal weights and matrix-free separable Kronecker-sum or
  Kronecker-product space-time operators with guarded small-reference
  materialization.
* Added temporal and spatiotemporal Moran statistics plus bounded temporal
  and joint space-time variograms with exact pair accounting.
* Added support-aware longitudinal change, per-element temporal trends, and
  mean/sum/integral/linear contrasts with explicit resource budgets.
* Added NGCS 3.3 schemas, language-independent corpus, migration/API/spec
  documents, tutorial, exact/adversarial tests, and large matrix-free release
  validation while retaining every earlier API.

# neurogeo 3.2.0

* Added immutable, resource-bound resampling plans that bind exact source and
  target domains to one already-selected transform path and explicit
  coverage, conservation, missing-support, measurement, and uncertainty
  policies.
* Added explicitly authorized nearest, trilinear, barycentric, and exact
  axis-aligned overlap support bridges for supplied non-lossy affine paths.
* Added joint path/plan/support-map diagnostics and result provenance,
  semantic change-of-support execution, optional variance propagation, and
  atomic one-artifact output integration.
* Added classed rejection of absent authorization, lossy/non-affine paths,
  incompatible methods, incomplete required coverage, plan mutation,
  uncertainty mismatch, and resource overruns.
* Added NGCS 3.2 schemas, conformance corpus, migration guide, tutorial, large
  bounded validation, and release governance while retaining all earlier APIs.

# neurogeo 3.1.0

* Added verified file-backed aligned values for NIfTI, CIFTI, MGH, and MGZ,
  with exact voxel/frame/brain-model/map selections and direct binary reads.
* Added deterministic bounded chunking, per-block resource enforcement,
  canonical cache/checkpoint identities, and source-mutation invalidation.
* Added partial-read provenance and metadata-only CIFTI parsing that does not
  load the complete CIFTI data matrix.
* Added bounded atomic pass-through copying for complete sources with explicit
  partial-selection rejection.
* Added the `ngcs/file-values` 3.1 schema, language-independent 3.1 corpus,
  large volume/91k-grayordinate validation, migration guide, and tutorial.

# neurogeo 3.0.0

* Added a versioned registry covering 18 core NGCS object schemas and their
  normative invariant sets.
* Added one structured schema-validation interface that delegates to the
  existing authoritative object validators and returns deterministic classed
  issues.
* Added canonical-JSON, SHA-256 object metadata manifests with atomic JSON
  writing, verification, corruption detection, and optional object matching.
* Added explicit schema-migration markers and a namespace-derived public API
  lifecycle registry that retains every 2.x export.
* Added the self-verifying, language-independent NGCS 3.0 conformance corpus.

# neurogeo 2.9.1

* Synchronized installed and source format documentation with the pure-R
  CIFTI writer, atomic BIDS derivatives, and support-map exchange schema 2.
* Added an installed-package conformance gate that verifies corpus discovery,
  fixture checksums, shipped specifications, and API inventory.
* Expanded the cross-platform CI gate so every configured platform validates
  the installed NGCS corpus after package checking.

# neurogeo 2.9.0

* Extended pure-R CIFTI dscalar, dlabel, and dtseries writing with declared
  float32, float64, and int32 datatypes, validated axes, label tables, and
  NamedMap metadata.
* Added canonical BIDS derivative name parsing/building, sidecar validation,
  atomic data-sidecar transactions, checksums, and explicit collision
  policies.
* Added chunked NGCS support-map exchange schema 2 bundles with per-file
  checksums, logical hashes, atomic promotion, schema-1 compatibility, and
  migration.
* Added a self-verifying, language-independent NGCS 1.0-2.9 JSON
  conformance corpus.
* Added cross-platform evidence and complete public-API inventories for the
  neurogeo 3.0 planning boundary. No 2.x API is deprecated in 2.9.

# neurogeo 2.8.0

* Added stable SHA-256 coordinate-space identities, exact registries, and
  explicit aliases that do not treat matching names as equivalence.
* Added field-level audits for kind, units, dimensionality, structure,
  template, density, and resolution.
* Added immutable directed transform graphs with edge hashes, mutation
  detection, inversion eligibility, and lossy-edge declarations.
* Added deterministic shortest-path search that rejects ambiguity until an
  exact edge sequence is selected.
* Added cycle, ambiguity, and edge-mismatch diagnostics plus serializable
  per-edge path provenance.
* Added explicitly authorized affine path application; no path estimates a
  transform, resamples data, or applies lossy/non-affine edges.

# neurogeo 2.7.0

* Added measurement-error-corrected variograms with fitted-parameter
  simulations and deterministic multi-worker results.
* Added kriging uncertainty decomposition across process, measurement,
  variogram-parameter, and declared support components.
* Added GWR local coefficient covariance, Gaussian intervals, and explicitly
  non-probabilistic bandwidth sensitivity ranges.
* Added Gaussian measurement-covariance simulations for SAR/SEM parameters
  and predictions.
* Added proper/intrinsic Gaussian CAR MAP estimates and posterior covariance
  under declared assumptions.
* Added within/between-support model ensembles and calibration summaries for
  bias, RMSE, coverage, interval width, and residual autocorrelation.

# neurogeo 2.6.0

* Added classed resource budgets, deterministic execution plans,
  identity-bound checkpoints, safe resume, and content-addressed caches.
* Added true blockwise change of support, diagnostics, independent variance
  propagation, and sparse composition without materializing input operators.
* Added delayed-native streaming summaries, covariance, sufficient-statistic
  linear regression, and Moran's I.
* Added atomic output helpers with final-file checksums and failure cleanup.
* Added randomized equivalence tests and a one-million-source bounded
  execution release gate.

# neurogeo 2.5.0

* Added a pure-R CIFTI-2 writer for dscalar, dlabel, and dtseries that
  preserves ordered brain models, vertex/voxel indices, affine, maps, labels,
  and time metadata without Connectome Workbench.
* Added callback/file-backed delayed aligned values and deterministic chunk
  iteration without introducing a second assay container.
* Added logically hashed row/column block sparse support maps with exact
  monolithic materialization and change-of-support equivalence.
* Added scoped BIDS derivative sidecars and writers with space, entities,
  measurement semantics, and provenance.
* Added pure-R golden round-trips and a one-million-source sparse/delayed
  construction, diagnostic, and change-of-support release gate.

# neurogeo 2.4.0

* Added bounded WLS spherical, exponential, and Gaussian variogram fitting.
* Added local ordinary/universal kriging with neighbor limits and prediction
  variance.
* Added deterministic GWR bandwidth CV, local coefficients, effective sample
  sizes, and condition numbers.
* Added unified OLS/SLX/SAR/SEM regression with guarded exact
  log-determinants and residual Moran diagnostics.
* Added foundational proper/intrinsic Gaussian CAR smoothing and declared
  cross-support model comparison.
* Added NGCS 2.4 simulation/reference validation and modelling documentation.

# neurogeo 2.3.0

* Added common-source permutation, Moran spectral, and surface-spin testing
  across declared support families, with explicit spatial-null claims and
  BH, BY, Holm, or common-simulation max-T adjustment.
* Added fixed and DerSimonian-Laird random-effects cross-atlas consensus,
  heterogeneity diagnostics, confidence intervals, and leave-one-atlas-out
  influence.
* Added caller-declared multiscale inference and segmentation-ensemble
  boundary tests with stable domain and operator identities.
* Enhanced atlas-robust effects with per-atlas confidence intervals,
  random-effects consensus, and an optional reproducible common-source
  paired-value bootstrap.
* Added independent max-T/meta-analysis references, null family-error
  calibration, known-effect bias/coverage validation, and NGCS 2.3
  conformance documentation.

# neurogeo 2.2.0

* Added domain-bound diagonal, matrix, and low-rank covariance objects with
  strict ordered-domain validation.
* Added analytic and Monte Carlo change-of-support uncertainty, including
  support-normalized intensive Jacobians and optional bounded full target
  covariance.
* Added validated operator, registration, and segmentation ensembles with
  normalized weights and integrity hashes.
* Added sparse conditioning, stable-rank, isolate, weak-support, entropy
  quantile, structure-coverage, and operator-variance diagnostics.
* Expanded sensitivity analysis with ensemble distributions and
  between-operator/within-value variance decomposition.
* Added analytic-versus-Monte-Carlo calibration and a 100k-source uncertain
  support performance gate.

# neurogeo 2.1.0

* Added sparse support-map builders for known surface registrations,
  affine voxel grids, axis-aligned voxel overlap, hard labels, and
  probabilistic atlases. No builder estimates registration.
* Added coverage, conservation, sparsity, membership-entropy, and operator
  uncertainty diagnostics plus bounded plotting methods.
* Added reproducible fixed-sparsity Monte Carlo operators, alternative-map
  sensitivity, and target-identity boundary sensitivity.
* Added atlas-robust support-weighted effects and common-source permutation
  comparisons with explicit non-invariance and non-spatial-null claims.
* Added Matrix Market plus JSON support-map exchange with ordered IDs,
  domain hashes, uncertainty, provenance, and integrity verification.
* Added NGCS 2.1 builder fixtures, GIFTI/NIfTI/CIFTI support workflows,
  inference calibration, and a 100k affine-grid construction gate.

# neurogeo 2.0.0

* Froze the language-independent NGCS 2.0 support-map specification while
  retaining all NGCS 1.0 dataset and five-domain invariants.
* Added sparse target-by-source `ngeo_support_map` objects for crisp,
  probabilistic, and overlapping support relationships, including strict
  domain hashes, coverage rules, composition, and migration from
  `ngeo_partition`.
* Added measurement-aware change of support: support-normalized intensive
  means, conservative extensive/count allocation, weighted categorical
  modes, and explicit unknown-semantics policy.
* Added first-order value/operator uncertainty propagation, with explicit
  rejection where overlap normalization would require an undeclared
  covariance model.
* Added cross-atlas intersection, Dice/Jaccard comparison, and auditable
  piecewise-constant transfer with a returned sparse operator and propagated
  variance.
* Added source-bootstrap global inference that verifies support-weighted
  means or totals across multiple parcellations.
* Added three language-independent NGCS 2.0 conformance fixtures and a
  100k-source/1k-target sparse performance gate.

# neurogeo 1.3.0

* Added stratified surface-spin nulls requiring explicit spherical
  registration coordinates.
* Added bounded Moran spectral randomization that preserves centered
  variance and graph autocorrelation within declared numerical tolerance.
* Added aligned OLS and spatial-lag-of-X regression with residual Moran
  diagnostics and strict domain/weights checks.
* Added explicit-bandwidth spatial kernel regression, including surface
  edge-geodesic distance and optional support weights.
* Added reproducible simulation streams across serial and PSOCK workers and
  release calibration for type-I error, coefficient bias, kernel recovery,
  and spectral invariants.

# neurogeo 1.2.0

* Added validation, composition, inversion, and geometry-only application of
  user-supplied affine transforms with strict source/target space checks.
* Added safe NIfTI, GIFTI, and FreeSurfer writers plus golden
  write/read/read round-trips; no writer invokes an external neuroimaging
  binary.
* Added optional KD-tree KNN, radius, and distance-band queries through the
  R `dbscan` backend, with exact small-data fallback and sparse edge guards.
* Added JSON-compatible provenance export with basename-only or full source
  redaction.

# neurogeo 1.1.0

* Added controlled, non-metric 2D surface charts and bounded one-way `sf`
  export that retains NGCS domain and distortion metadata.
* Added base-graphics diagnostics for all five domains, weights, and
  partitions.
* Added Getis-Ord Gi/Gi-star and exact-order sparse spatial correlograms,
  validated against `spdep`.
* Added reusable permutation controls, one- and two-sided inference, and
  multiple-testing adjustment for local statistics.

# neurogeo 1.0.1

* Added classed `ngeo_error_io` failures for malformed NIfTI, GIFTI, CIFTI,
  FreeSurfer, mask, and JSON sidecar inputs.
* Added malformed/truncated input regression tests and classed backend
  warnings.
* Expanded the prepared CI matrix to R release, devel, and oldrel across
  Linux, macOS, and Windows.
* Made local release construction append-only by snapshotting each run,
  its validation reports, check output, session information, and checksums.

# neurogeo 1.0.0

* Froze the language-independent NGCS 1.0 specification and stable 1.0 API.
* Added the Phase 0 controlled-S3 object prototype.
* Added surface, volume, and grayordinate conformance fixtures.
* Added constructors for points, grayordinates, and regions.
* Added accessors, domain hashing, transform and measurement metadata.
* Added synchronized element/map subsetting and provenance records.
* Added NIfTI, GIFTI, CIFTI, and FreeSurfer format adapters with no external
  binary runtime dependency.
* Added synthetic golden files for NIfTI, GIFTI, CIFTI dscalar/dlabel/dtseries,
  FreeSurfer surfaces, curv, annot, and MGH.
* Added sparse surface, voxel, region, and grayordinate block adjacency.
* Added explicit Euclidean, world-space, edge-geodesic, and hop distances.
* Added sparse contiguity, KNN, distance-band, inverse-distance, and Gaussian
  weights with component diagnostics and optional spdep/igraph converters.
* Added domain-bound crisp partitions, explicit background handling, boundary
  extraction, region adjacency, and measurement-aware regional aggregation.
* Added support-weighted intensive aggregation, conservation-preserving
  extensive/count sums, categorical modes, and auditable aggregation
  provenance.
* Added global Moran's I, Geary's C, Local Moran/LISA, reproducible
  permutation inference, empirical variograms, and diagnostic plots.
* Added four workflow vignettes, a pkgdown site, external-data validation,
  32k/91k/164k performance regression coverage, governance, and reproducible
  local release artifacts.
