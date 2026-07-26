# Distribution route decision

Status: accepted for 1.0 planning

## Decision

Target CRAN as the primary public package distribution route after the local
1.0 release candidate has passed policy and platform checks. Maintain a
Bioconductor-compatible dependency posture, but do not make Bioconductor the
1.0 release gate.

## Rationale

`neurogeo` is a general R spatial-data infrastructure package. Its core API
uses standard S3 objects and sparse `Matrix` values, and its optional
neuroimaging readers already rely on CRAN-distributed backends. CRAN gives
the broadest installation path without requiring the package to adopt a
Bioconductor experiment-container abstraction that conflicts with the
one-domain/one-values-block contract.

Bioconductor remains a possible later route if interoperability with its
neuroimaging or experiment ecosystems becomes a primary product goal. Such a
submission would be evaluated separately and must not change NGCS semantics.

## Release consequences

- Keep all external neuroimaging binaries optional and outside runtime.
- Treat R CMD check on current release/devel R and three operating systems as
  a release gate.
- Keep examples and vignettes runnable without downloading private datasets.
- Do not publish automatically; repository creation and public submission
  require explicit maintainer authorization.
