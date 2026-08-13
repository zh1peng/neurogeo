# Contributing to neurogeo

neurogeo accepts focused changes that preserve its core contract: one
spatial domain, one aligned values block, explicit spatial and measurement
semantics, bounded computation, and auditable provenance.

## Before changing code

Open an issue for a new scientific method, object invariant, public API, or
file contract. State the domain, indexing, space, topology, metric, support,
measurement semantics, transform assumptions, provenance, and expected
resource behavior.

Do not add implicit preprocessing, registration, resampling, segmentation,
surface reconstruction, or dependencies on external neuroimaging binaries.

## Local setup

Install the suggested packages, then install neurogeo with its tests:

```powershell
R.exe CMD INSTALL --install-tests --library=.r-lib .
Rscript tools/run-unit-tests.R
```

Before pushing a behavior change to `main`, run:

```powershell
powershell -ExecutionPolicy Bypass -File tools/run-validation-suite.ps1
```

## Tests

Every behavioral change needs:

- a positive reference result;
- a classed failure for an invalid contract where applicable;
- deterministic seeds for simulation;
- explicit numerical tolerances;
- a resource-bound test when materialization can grow with data size.

Format changes additionally need round-trip, metadata, indexing, truncated
input, and source-mutation tests.

## Documentation

Public functions require R documentation. User-facing workflows belong in a
vignette and must be executable unless an external file is intentionally
required. Update `NEWS.md` for every release-visible change.

## Development and review workflow

Keep changes narrow and submit them through a pull request; do not push a
science or public-API change directly to protected `main`. Explain scientific
impact, user impact, validation, and migration, and do not mix unrelated
cleanup with a scientific change.

Every science or public-API change requires approval from at least one reviewer
who did not author the change. Scientific changes must update the inference
contract and its executable validation before release. A maintainer
may merge documentation-only or CI-maintenance changes after the relevant
automated gates pass. Version tags and GitHub Releases additionally require the
release attestation and the release-owner approval.
