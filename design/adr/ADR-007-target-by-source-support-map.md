# ADR-007: Support maps are sparse target-by-source operators

Status: accepted for NGCS 2.0

## Decision

Represent every change-of-support relationship as a non-negative sparse
matrix with target elements in rows and source elements in columns. Bind the
matrix to ordered source and target domain hashes and keep it independent of
the one-domain/one-values dataset.

## Consequences

- Applying an operator reads naturally as `target = A %*% source`.
- Crisp, probabilistic, and overlapping mappings share one validation path.
- Intensive and extensive semantics can use different, explicit algebra.
- Composition is ordinary sparse multiplication with intermediate-domain
  validation.
- Atlas overlap is computed from common source support.
- A support map is not a hidden registration or a parcel reconstruction
  model.

The orientation is normative and must never be inferred from dimensions.
