# neurogeo 4.4.1 to 4.4.2 maintenance plan

Status: completed

## Overall objective

Stabilize neurogeo without adding a scientific module, public object model,
or exported function. Version 4.4.1 closes concrete resource-safety and I/O
consistency gaps. Version 4.4.2 removes maintenance duplication and makes the
current validation and specification sources unambiguous.

Development is committed directly to `main`. No pull request, version tag, or
GitHub Release is created.

## 4.4.1: safety and consistency

1. Bound every exact dense CAR path before matrix inversion or covariance
   materialization.
2. Make `write_ngeo()` recognize and dispatch CIFTI dscalar, dlabel, and
   dtseries paths consistently with `read_ngeo()`.
3. Use the exported `xml2::xml_attrs()` interface instead of namespace
   internals.
4. Add regression tests for the corrected paths and directly exercise public
   functions that previously had no direct test call.

Exit criteria:

- oversized exact CAR and CAR-uncertainty requests fail with
  `ngeo_error_resource` before dense work;
- generic CIFTI output round-trips scalar and label data;
- no public export is added;
- focused model, I/O, transform, spatiotemporal, accessor, and BIDS tests pass.

## 4.4.2: maintenance consolidation

1. Remove obsolete source-archive/Release construction code and tracked
   historical check logs.
2. Rename the local validation-suite entry point so validation is not confused
   with GitHub Releases.
3. Stop tracking website copies of cortical figures already copied from the
   vignette source by the website renderer.
4. Declare `inst/spec/` the canonical installed contract and `design/` the
   location for development plans, audits, risks, and frozen historical
   rationale. New package contracts are not mirrored in both locations.
5. Update package metadata and maintenance documentation without renaming
   unrelated historical tests or refactoring stable scientific code.

Exit criteria:

- version is 4.4.2 and NEWS records both maintenance stages;
- obsolete Release builder/logs and redundant tracked website figures are
  absent;
- the validation suite, package tests, installed conformance, full performance
  gates, tutorials, website build, and `R CMD check` pass;
- local and remote `main` match, with no PR, tag, or GitHub Release.

## Completion record

Completed on 2026-07-31. Focused and full unit tests, installed conformance,
the complete validation suite, large-data performance tests, tutorial
rendering, VitePress build, and `R CMD check --as-cran --no-manual` passed.
The check reported no errors or warnings; its two notes were the expected new
submission notice and inability to verify the current time.
