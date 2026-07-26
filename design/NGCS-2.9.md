# Neuroimaging Geoinformatics Core Specification 2.9 addendum

Status: stable  
Version: 2.9  
Base specification: NGCS 2.8

NGCS 2.9 completes the 2.x interoperability contract and prepares an
auditable boundary for 3.0. All NGCS 1.0-2.8 contracts remain.

A supported CIFTI writer MUST validate its map axis, brain models, label
tables, and declared datatype before writing. Dense scalar maps MAY use
float32 or float64. Dense label maps MUST use int32 and a label axis with
finite RGBA values in [0, 1]. Dense series maps MUST use an equally spaced
time axis with a supported unit. NamedMap metadata applies only to named-map
axes and MUST round-trip as ordered key-value mappings. Unsupported axis
contracts MUST fail explicitly.

A BIDS derivative name MUST be parsed and built from ordered, validated
entities, one suffix, and one supported extension. Its sidecar MUST bind the
exact NGCS domain identity, space, measurement semantics, entities, and
provenance. Data and sidecar MUST be promoted as one transaction. Existing
outputs MUST be handled by an explicit error, overwrite, or deterministic
version policy. This contract does not orchestrate a BIDS dataset.

Support-map exchange schema 2 is an atomic directory bundle. Its manifest
MUST declare ordered, contiguous source-column chunks, dimensions, nonzero
counts, file sizes, per-file SHA-256 checksums, and the complete logical
support-map hash. A reader MUST validate all checksums before reconstruction
and MUST reproduce the schema-1 logical operator and hash. Schema 1 remains
readable and has an explicit migration path. Matrix Market and JSON remain
the exchange formats; NGCS does not define a custom binary format.

The NGCS conformance corpus MUST be self-describing, versioned,
language-independent JSON. Its manifest MUST enumerate the covered
specifications and bind every fixture by schema and SHA-256.

The 2.9 public API inventory MUST identify every exported symbol and any
planned 3.0 action. No 2.x API is removed or deprecated in 2.9. Platform
evidence MUST distinguish completed local validation from configured remote
CI; configuration alone is not execution evidence.

Conformance requires CIFTI round-trips, valid and adversarial BIDS fixtures,
atomic failure and collision tests, schema-1/schema-2 equivalence, checksum
mutation rejection, corpus self-verification, and a complete API inventory.
