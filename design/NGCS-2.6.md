# Neuroimaging Geoinformatics Core Specification 2.6 addendum

Status: stable  
Version: 2.6  
Base specification: NGCS 2.5

NGCS 2.6 standardizes bounded execution over the existing one-domain,
one-aligned-values-block model. All NGCS 1.0-2.5 contracts remain.

A resource budget declares finite limits before execution. An operation MUST
fail with a classed condition when its declared task, block, byte, element, or
elapsed-time requirement exceeds that budget. It MUST NOT silently replace a
sparse or delayed path with a dense whole-domain fallback.

An execution plan is an ordered set of deterministic tasks bound to an
identity that includes every domain, operator, values, measurement-semantic,
and operation parameter relevant to the result. A checkpoint MUST bind the
same identity and plan hash. Resume MUST reject mutation and MUST neither skip
nor repeat completed tasks.

A block support operation consumes the stored sparse blocks directly.
Change of support, diagnostics, independent variance propagation, and
composition MUST preserve target-by-source orientation, ordered identities,
measurement semantics, allocation rules, and the logical operator result.
Input operators MUST NOT be reconstructed as complete monolithic matrices.

A streaming operation reads deterministic aligned value slices. Streaming
summary, covariance, regression sufficient statistics, and Moran's I MUST
equal their in-memory references within declared numerical tolerance.

A content cache is keyed by a canonical identity and records its digest.
Atomic output MUST be written to a sibling temporary path, promoted only after
successful completion, and accompanied by a checksum. Failure MUST leave no
partial final output.

Conformance requires randomized block/monolithic and delayed/in-memory
equivalence, deterministic resource rejection, interruption/resume tests,
cache-identity tests, atomic-failure tests, and a one-million-source sparse
execution gate.
