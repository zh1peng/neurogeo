# ADR-003: Pure-R core CIFTI importer

Status: accepted

## Decision

The core CIFTI reader will use the pure-R `cifti` package and will not call
Connectome Workbench. `ciftiTools` may be an optional interoperability
backend.

## Consequences

CIFTI dscalar, dlabel, and dtseries must be usable without an external
binary. Golden fixtures must cover matrix and brain-model mappings.

