# Neuroimaging Geoinformatics Core Specification 3.5

Status: stable  
Version: 3.5  
Base specification: NGCS 3.4

NGCS 3.5 defines reproducible, auditable analysis replay and portable
artifact integrity. It does not define a general workflow language.

A logical object hash MUST cover the ordered domain, aligned scientific
values, maps, measurement semantics, labels, and file-backed source
identity. Incidental execution timestamps MUST NOT affect logical identity.
Materializing values for hashing MUST obey an explicit resource budget.

An `ngcs/provenance-dag` MUST contain unique node identifiers, content
hashes, existing directed parents, and explicit edge roles. It MUST be
acyclic and carry an immutable canonical identity.

An `ngcs/replay-manifest` MUST declare its software environment, named input
hashes, ordered operations, argument-to-artifact bindings, expected
intermediate and output hashes, and complete provenance DAG. Implementations
MUST execute only a published operation whitelist and MUST NOT evaluate
embedded code. Replay MUST fail before returning results when inputs,
environment, intermediate results, or outputs differ.

An `ngcs/artifact-manifest` MUST contain only root-relative paths and MUST
record byte size, SHA-256, semantic role, and completeness for every
artifact. An `ngcs/batch-manifest` MUST have `derivative_only` scope and
MUST reference a complete artifact manifest. Readers MUST verify missing,
incomplete, or corrupt artifacts before use.

Artifact batches MUST be staged out of view and published as one directory
transition. A failed writer MUST NOT expose a target manifest or partial
batch. BIDS entities are descriptive derivative metadata only; NGCS 3.5
does not orchestrate a BIDS dataset.
