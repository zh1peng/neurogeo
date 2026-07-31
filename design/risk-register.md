# Risk register

Status: reviewed for neurogeo 4.4.2

| Risk | Current status | Mitigation |
|---|---|---|
| Pure-R CIFTI coverage incomplete | controlled | dscalar/dlabel/dtseries golden tests and a 59,412-grayordinate external workflow |
| Object model becomes too complex | controlled | five domains; one domain and one aligned values block |
| Object model too simple | controlled | explicit capabilities; regions and grayordinates are first-class |
| 32k/164k memory growth | controlled | sparse topology, bounded distances, 32k/164k regression runs |
| A viewing projection is presented as metric flattening | controlled | chart kind and `is_metric_flattening = FALSE` are mandatory for orthographic, PCA, and spherical views |
| A closed cortex is silently cut | controlled | harmonic parameterization requires an explicit complete disk boundary and rejects closed/multi-boundary topology |
| A spherical seam creates false cross-map polygons | controlled | record seam-crossing edges/faces and render wrapped copies clipped at both longitude limits |
| A folded or highly distorted chart is hidden | controlled | retain per-face signed area, fold, area ratio, and angular-error diagnostics plus summary metadata |
| Atlas labels select a hidden template geometry | controlled | atlas-independent source charts; exact vertex alignment and source-domain hash checks |
| A partition is reused after unrelated domain mutation | controlled | require the current domain hash or the exact source hash recorded by the selected chart |
| Face interpolation is mistaken for source measurement | controlled | retain source vertex values and identify face means/modes as rendering data only |
| Cortical rendering exhausts memory | controlled | one face table, vectorized base rendering, no dense pairwise distance, and real 32k resource validation |
| Shared panels imply comparable scales when palettes differ | controlled | shared continuous layouts require one palette and pool one finite face-value range |
| Shared atlas labels silently receive conflicting colors | controlled | reject the layout when one categorical label has more than one declared color |
| QC is mistaken for structural validation or automatic repair | controlled | `ngeo_validate()` remains authoritative; QC is non-mutating and reports risks only |
| QC materializes a large values block | controlled | explicit `max_value_cells` gate and `not_evaluated` status before materialization |
| QC densifies topology or support operators | controlled | sparse adjacency/support summaries and an explicit topology element guard |
| Exact CAR allocates dense matrices beyond a reviewed bound | controlled | shared exact-model dimension guard before smoother, precision, or covariance construction |
| Package contracts diverge between two source directories | controlled | `inst/spec/` is canonical; `design/` retains plans and frozen historical rationale only |
| Public API expands faster than it can be maintained | controlled | 4.4.1 and 4.4.2 add no exports or schemas; additions require an explicit scientific use case |
| Missing coordinate-space metadata | accepted | explicit `unknown`; no implicit conversion |
| Incorrect measurement semantics | controlled | unknown-by-default and aggregation refusal without `fun` |
| CIFTI surface mismatch | controlled | structure, vertex-count, and mapping validation |
| Backend API change | monitored | isolated adapters and cross-platform CI matrix |
| Scope expands into a general toolbox | controlled | frozen non-goals and API review |
| Reference result disagreement | controlled | 4.2 comparisons with spdep, spatialreg, gstat, and GWmodel plus seeded calibration |
| Reference package updates change fitted results | monitored | record exact reference versions and retain explicit numeric tolerances in every 4.2 report |
| Calibration gates are mistaken for universal validity | controlled | publish matched estimands, simulation design, and explicit non-claims with the installed specification |
| Low adoption | open | native constructors, common readers, vignettes, converters |
| Large public API is difficult to discover | controlled | publish stable core, advanced, and exchange/governance tiers; require runnable examples for core workflows |
| Coverage percentage is mistaken for scientific validity | controlled | use coverage only as a regression signal and retain independent reference, calibration, invariant, and real-data gates |
| An upstream package license is mistaken for a data license | controlled | record package license and original data terms separately; keep 4.2.2 fixtures download-only |
| Validation silently loads an older installed neurogeo | controlled | prepend the project-local library in every validation and build script; report the loaded package version |
| A scale claimed by the plan lacks a compatible fixture | controlled | machine-readable exercised/not-exercised scale inventory; no synthetic result is presented as real-data evidence |
| Cross-platform behavior diverges | controlled | Windows, macOS, Ubuntu release, oldrel, and devel checks are distribution gates |
| Overlapping support duplicates extensive mass | controlled | reject non-unit allocation unless explicit column normalization is selected |
| Atlas transfer presented as inverse reconstruction | controlled | require and record the piecewise-constant model; expose the transfer operator and uncertainty |
| Support operators become dense | controlled | target-by-source sparse representation, composition guards, and 100k-by-1k performance regression |
| Uncertain normalized overlap ignores covariance | controlled | reject this case until an explicit covariance model is supplied |
| Parcellation invariance is overclaimed | controlled | limit the claim to global support-weighted means and totals, with numerical verification |
| Numeric proximity is mistaken for registration | controlled | require a common known space or explicit known-registration identifier; never estimate registration |
| Barycentric candidate search misses the global closest triangle | documented | exact search for bounded inputs; record the bounded candidate engine and projection distance for large inputs |
| Rotated voxel overlap is labelled exact | controlled | reject rotation, shear, and axis permutation in the exact overlap builder |
| Atlas probabilities are transposed twice | controlled | accept source-by-region input and test the normative target-by-source fixture |
| Uncertainty sampling changes sparsity silently | controlled | fixed sparsity, explicit truncation/normalization assumptions, seeded draws, and validation per draw |
| Common-source permutations are mistaken for spatial nulls | controlled | return an explicit non-spatial-null claim and retain separate spin/Moran null APIs |
| Diagnostic density overflows at scale | controlled | double-precision dimension products and a 100k construction/diagnostic gate |
| Covariance silently misaligns after subsetting | controlled | bind covariance to ordered domain hash and element IDs |
| Full covariance becomes dense | controlled | diagonal-by-default propagation and explicit target/draw dimension guards |
| Operator alternatives are treated as calibrated probabilities | controlled | typed ensemble provenance and sensitivity-range language |
| Normalized means ignore numerator/denominator dependence | controlled | use the exact support-normalized Jacobian and Monte Carlo calibration |
| Max-T simulations are generated independently by atlas | controlled | create one source-domain realization per row and reuse it across the full support family |
| Consensus is mistaken for local parcellation invariance | controlled | label results as family-specific meta-analysis and report heterogeneity and leave-one-atlas-out influence |
| Unconstrained permutations are mistaken for spatial nulls | controlled | record a spatial-preservation flag and expose Moran spectral and surface-spin alternatives |
| Multiscale order is inferred from labels | controlled | require a unique caller-declared ordered scale vector |
| Kriging allocates whole-domain dense covariance | controlled | local neighbor cap and returned neighbor count |
| GWR silently accepts singular local fits | controlled | classed failure or declared NA plus local condition numbers |
| SAR/SEM changes log-determinant algorithm silently | controlled | record exact method/tolerance and enforce a dimension guard |
| Intrinsic CAR is treated as proper | controlled | record impropriety and sum-to-zero constraint |
| Delayed storage becomes a multi-assay abstraction | controlled | one fixed element-by-map block and one map table |
| Block partition changes operator order | controlled | complete ordered row/column groups and logical hash verification |
| CIFTI writer depends on Workbench | controlled | direct pure-R CIFTI-2/NIfTI-2 writer and golden round-trips |
| BIDS support expands into orchestration | controlled | write only explicitly named derivatives and relevant sidecars |
| Million-element validation densifies the operator | controlled | sparse Matrix blocks, delayed callback values, and resource report |
| Block execution reconstructs the logical operator | controlled | direct block accumulation plus provenance flag and randomized equivalence tests |
| Resume accepts mutated scientific inputs | controlled | checkpoint plan hash binds the caller-supplied complete identity |
| Cache reuses semantically different results | controlled | canonical cache identity must include semantics and every provenance-relevant parameter |
| Failed output leaves a valid-looking partial file | controlled | sibling temporary output, atomic promotion, checksum, and failure-cleanup tests |
| Streaming statistics drift from in-memory references | controlled | stable sufficient statistics and delayed/in-memory equality tests |
| Model covariance is attached after domain mutation | controlled | exact ordered domain-hash and element-ID validation before every 2.7 model |
| Variogram measurement correction produces negative semivariance | controlled | retain raw values and explicitly record zero truncation |
| Independent uncertainty components are double counted | documented | return every component separately and state the independence assumption for totals |
| GWR bandwidth sensitivity is presented as inferential uncertainty | controlled | label ranges as sensitivity and explicitly reject confidence-interval wording |
| SAR/SEM results vary with worker scheduling | controlled | pre-generated seeded draws, ordered simulation results, and 1/2-worker equality tests |
| Deterministic CAR smoothing is described as Bayesian | controlled | reserve posterior language for explicit Gaussian covariance and precision in `ngeo_car_uncertainty()` |
| Cross-support ensemble is mistaken for invariance | controlled | law-of-total-variance output scoped to the declared support family |
| Matching space names are treated as equivalence | controlled | normative space hashes and ambiguity on duplicate names |
| Alias silently changes its target | controlled | exact hash binding, conflict rejection, and registry integrity hash |
| Transform graph estimates a missing registration | controlled | graph accepts supplied edges only and path failure is terminal |
| Ambiguous path depends on insertion order | controlled | collect shortest candidates and require exact caller selection |
| Lossy transform is silently inverted or applied | controlled | separate invertible/lossy flags and application refusal |
| Transform edge mutates after path creation | controlled | per-edge transform hashes plus graph and path hashes |
| Unit, structure, density, or resolution mismatch is hidden | controlled | field-level space and edge audits retained in diagnostics |
| CIFTI metadata is silently lost or written with an incompatible datatype | controlled | validate NamedMap, label, time-axis, and datatype contracts before pure-R writing and round-trip the supported corpus |
| BIDS data and sidecar become an inconsistent partial pair | controlled | sibling transaction directory, collision policy, checksums, promotion rollback, and failure-cleanup tests |
| Chunked support exchange is reordered or corrupted | controlled | ordered contiguous ranges, per-chunk SHA-256, complete logical hash, and schema-1/2 equivalence tests |
| Local validation is presented as cross-platform proof | controlled | compatibility inventory distinguishes local evidence from configured remote CI evidence |
| Schema registry diverges from authoritative validators | controlled | generic validation delegates to the existing object-specific validator and records the checked invariant set |
| Portable manifest depends on R serialization | controlled | canonical JSON SHA-256 over normative fields with an explicit language-independent corpus |
| A 3.0 release silently removes a 2.x API | controlled | namespace-derived lifecycle inventory, retain policy, compatibility tests, and explicit migration dispatch |
| Manifest metadata is mistaken for a second values block | controlled | manifests contain storage/dimension/semantic identity only and never copy aligned values |
| A file-backed reader hides complete-file materialization | controlled | direct binary addressing, metadata-only CIFTI parsing, bounded compressed decoding, and large-file resident-object gates |
| File mutation reuses stale cache or checkpoint output | controlled | identity binds checksum or explicit metadata state plus binary contract and exact selections; every read revalidates the chosen policy |
| A partial file-backed selection is copied as if complete | controlled | pass-through output requires complete selection and matching suffix; partial sources raise a classed condition |
| One large indexing request defeats bounded execution | controlled | resource budget is checked on every materialized block before binary I/O |
| A resampling plan estimates or chooses registration | controlled | plans accept one already-selected exact transform path and require separate explicit execution authorization |
| Lossy/non-affine paths are treated as executable interpolation | controlled | 3.2 bridge rejects every lossy or non-affine-applicable path with a classed condition |
| Geometric coverage is confused with extensive conservation | controlled | separate coverage, missing-support, and conservation policies with independent adversarial tests |
| Transform and support provenance diverge | controlled | plan, path, support-map, and joint SHA-256 identities are repeated in diagnostics and result provenance |
| Resampling silently exceeds memory | controlled | pre-build sparse nonzero estimates and pre-materialization result budgets fail before their respective allocations |
