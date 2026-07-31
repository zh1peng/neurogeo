# neurogeo 4.3.2 to 4.4 development plan

Status: complete

## Final objective

Stabilize real cortical cartography for routine bilateral and multi-panel
figures, then add one bounded quality-control entry point that reports
scientific risks in otherwise valid NGCS objects.

Development is performed directly on `main`. This roadmap does not use pull
requests, create Git tags, or publish GitHub Releases.

## Design constraints

- keep one spatial domain and one aligned values block;
- preserve source vertex, face, space, measurement, and provenance identity;
- add no registration, resampling, preprocessing, or atlas-specific geometry;
- add no plotting framework dependency;
- add one QC public function rather than a family of diagnostic wrappers;
- never materialize a large values block or dense spatial matrix implicitly;
- keep NGCS 3.5 object schemas and scientific estimators unchanged.

## Stage 1: neurogeo 4.3.2 cortical-cartography stabilization

### Goal

Make bilateral and multi-panel cortical figures use one auditable scale and
legend while retaining the existing atlas-independent map contract.

### Deliverables

- add `shared_scale` to `ngeo_cortical_layout()`;
- pool continuous limits and recolor every panel from the same scale;
- preserve categorical atlas colors and ordering, rejecting conflicting
  colors for the same label;
- draw one shared continuous or categorical legend for the layout;
- retain panel-specific source domains, face mappings, masks, underlays, and
  provenance;
- synchronize current version, roadmap, risk, README, and tutorial text;
- validate continuous vertex, categorical vertex, atlas, missing-value, and
  incompatible-layout behavior.

### Exit criteria

- shared continuous limits and categorical legends are deterministic;
- a conflicting categorical color contract fails before rendering;
- ordinary layouts remain backward compatible;
- semantic and portable rendering tests pass;
- the existing real HCP Conte69/Schaefer validation remains valid.

## Stage 2: neurogeo 4.4.0 unified scientific QC

### Goal

Provide one non-mutating diagnostic report for scientifically important
conditions that do not necessarily make an NGCS object structurally invalid.

### Public contract

`ngeo_qc(x, support_map = NULL, chart = NULL, tolerance = 1e-8,
max_value_cells = 1e6)` returns one `ngeo_qc` object.

The report contains a machine-readable check table plus bounded details for:

- domain, element, map, and value alignment;
- known versus unknown coordinate space;
- measurement semantics and units;
- missing, non-finite, and constant value maps when the values block is
  within the explicit scan budget;
- sparse topology components and isolates when topology is available;
- selected cortical-chart coverage, folds, and distortion metadata;
- optional support-map coverage and conservation;
- provenance presence.

`print()` provides a short status summary and `plot()` displays status counts.
No additional public QC sub-functions are introduced.

### Status semantics

- `pass`: the check found no declared concern;
- `info`: descriptive result or a condition that can be scientifically valid;
- `warning`: analysis should not proceed without an explicit decision;
- `not_evaluated`: the required data exceed the declared budget;
- `not_applicable`: the object lacks that optional capability.

QC is not validation. `ngeo_validate()` remains authoritative for structural
invariants. QC does not infer missing metadata or repair an object.

### Exit criteria

- all five domain types produce deterministic QC reports;
- unknown space, unknown semantics, missing/non-finite values, constant maps,
  disconnected topology, chart folds, incomplete/non-conservative support,
  and budget skips are tested;
- topology and support diagnostics remain sparse;
- report generation does not mutate its inputs;
- documentation, Chinese tutorial, full unit tests, release performance
  gates, and `R CMD check --as-cran` pass.

## Deferred

- group or repeated-measures inference;
- automatic label placement optimization;
- interactive viewers and web-rendering frameworks;
- automatic cortical cuts, registration, or resampling;
- clinical validation and preprocessing.

## Completion evidence

- focused 4.3.2 and 4.4 tests pass;
- the complete unit/integration suite passes;
- all 33 large sparse performance assertions pass;
- installed 4.4 conformance passes;
- Chinese and English QC tutorials execute;
- VitePress documentation builds;
- Windows R 4.4.3 `R CMD check --no-manual` reports `Status: OK`;
- no pull request, version tag, or GitHub Release is part of this roadmap.
