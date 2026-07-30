# neurogeo API 4.2.2

Version 4.2.2 adds no public function, class, argument, return field, NGCS
schema, or numerical estimand.

It adds a release-validation surface:

- `inst/extdata/reference-4.2.2/manifest.csv` records immutable external
  fixture identities, original data terms, package licenses, byte sizes,
  SHA-256 values, and a download-only policy;
- `tools/fetch-reference-422.R` creates a verified ignored cache;
- `tools/run-real-data-validation-422.R` executes four real-data workflows
  and emits `real-data-422-validation.json`.

The validation surface is release tooling, not runtime API. NIfTI, GIFTI,
FreeSurfer, CIFTI, support-map, measurement-semantics, and provenance
contracts remain those documented for 4.1, 4.2, and NGCS 3.5.
