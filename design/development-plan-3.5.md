# neurogeo 3.5 development target

## Goal

Complete the 2.9.1-3.5 roadmap with a portable, auditable replay layer for
bounded neurogeo analyses. Replay is a verification mechanism for declared
neurogeo operations, not a general workflow engine.

## Modules

1. Scientific logical hashes that cover domain, aligned values,
   measurement semantics, labels, and file identities while excluding
   incidental provenance timestamps.
2. Immutable provenance DAGs with unique nodes, existing parents, explicit
   edge roles, cycle detection, and canonical SHA-256 identity.
3. Environment snapshots and replay manifests with named inputs, ordered
   whitelist-only steps, expected intermediate/final hashes, and exact or
   compatible environment policy.
4. Portable root-relative artifact manifests with size, SHA-256, role,
   completeness, and verification before use.
5. Atomically published derivative-only artifact batches that never expose
   a manifest or partial target directory after a writer failure.
6. NGCS schemas/corpus, migration/API/specification documents, tutorial,
   adversarial tests, end-to-end validation, release evidence, and final
   2.9.1-3.5 audit.

## Exit criteria

- A reference time-aware analysis records and replays to identical logical
  hashes for every declared output.
- Input mutation, environment drift, missing parents, cycles, unsupported
  operations, output drift, incomplete batches, and artifact corruption are
  classed visible failures.
- Replay manifests contain data and declared operations only; they cannot
  embed or evaluate arbitrary R code.
- Artifact paths are root-relative and BIDS scope remains explicitly
  `derivative_only`.
- Failed batch writers leave no visible target directory or manifest.
- neurogeo 3.5.0 archive, validation reports, SHA-256, documentation,
  `R CMD check --as-cran: Status OK`, and the complete roadmap evidence
  audit are finished.
