# neurogeo 3.2 development target

## Goal

Complete an explicitly authorized bridge from an already-supplied transform
path to a sparse support map and measurement-aware resampling result. No API
may estimate registration, select an ambiguous path, silently invert an edge,
or resample merely because two space names look compatible.

## Modules

1. `ngeo_resampling_plan` binds exact source/target domains, a validated
   transform path, compatible interpolation method, coverage/conservation/
   missing/uncertainty policies, resource budget, and complete plan identity.
2. A support bridge applies only an authorized, non-lossy affine path and
   delegates to the existing nearest, trilinear, barycentric, or exact
   axis-aligned overlap builder.
3. Joint diagnostics report sparse size, source coverage, empty targets,
   support conservation, path identity, map identity, and deterministic
   provenance hash.
4. Measurement-aware execution delegates to the existing support-change and
   variance engines, enforces pre-materialization budgets, and optionally
   promotes one caller-written output atomically.
5. NGCS 3.2 schemas, language-independent conformance corpus, API/migration
   documentation, tutorial, adversarial tests, exact references, and large
   bounded validation complete the release.

## Exit criteria

- Identity paths and direct small references agree exactly; affine nearest,
  trilinear, barycentric, and overlap references agree within their declared
  tolerance.
- Intensive normalization and extensive conservation are demonstrated with
  explicit measurement semantics.
- Missing coverage, unsupported domain/method combinations, lossy or
  non-affine paths, absent authorization, changed domains/plans, invalid
  uncertainty inputs, and insufficient budgets fail with stable classed
  conditions.
- Transform-path and support-map hashes are jointly recorded in both
  diagnostics and result provenance.
- No function estimates registration or performs implicit resampling.
- The archive, validation reports, SHA-256, documentation site, and
  `R CMD check --as-cran: Status OK` provide complete neurogeo 3.2.0
  evidence before 3.3 begins.
