# Neuroimaging Geoinformatics Core Specification 3.1

Status: stable  
Version: 3.1  
Base specification: NGCS 3.0

NGCS 3.1 standardizes file-backed aligned values for bounded analysis of
NIfTI, CIFTI, MGH, and MGZ artifacts. It does not introduce another domain,
another values block, or a multi-assay container.

## File-backed values

An `ngcs/file-values` object MUST represent exactly one two-dimensional
element-by-map values block. Its selected rows MUST align one-to-one and in
order with the enclosing spatial domain, and its selected columns MUST align
one-to-one and in order with the map and measurement tables.

The object MUST declare source format, absolute source identity, binary
offset, datatype, endianness, scaling, source-axis layout, selected source
indices, map names, dimensions, mutation-verification policy, and resource
budget. It MUST remain a valid `ngcs/delayed-values` implementation.

## Bounded reads

NIfTI and MGH/MGZ readers MUST address voxels and frames directly from their
binary layouts. CIFTI readers MUST address the declared brain-model and map
axes directly from the NIfTI-2 payload. Uncompressed sources SHOULD use
seekable contiguous runs. Compressed sources MUST decode sequentially with a
bounded block and MUST NOT retain the complete decompressed payload.

Every requested materialized block MUST pass its resource budget before
binary reading. Row and map selections MUST preserve caller order. Chunk
iteration MUST be deterministic and produce the same ordered matrix as an
in-memory reader.

## Identity, mutation, and provenance

A file-backed identity MUST bind canonical source path, size, modification
time, optional source SHA-256, format, binary contract, exact selections,
dimensions, and map names. Selection changes MUST change the identity.

Checksum verification MUST reject altered bytes. Metadata verification MUST
reject changed size or modification time. Verification MAY be disabled only
by an explicit caller choice. Reads MUST record the incomplete-read state,
source indices, verification policy, and file identity in provenance.
Cache and checkpoint consumers MUST use this complete identity.

## Output boundary

Version 3.1 provides bounded atomic pass-through copying for a complete
file-backed selection. The writer MUST copy through a sibling temporary
artifact, promote only after success, return the final checksum, and reject
format-suffix changes. A partial selection MUST be rejected because copying
it would falsely publish unselected source data.

This pass-through API is not a general subset encoder. Existing format
writers remain authoritative when values are intentionally materialized and
encoded as a new artifact.

## Conformance

Conformance requires in-memory/file-backed equality for NIfTI, CIFTI,
MGH/MGZ, exact selection alignment, deterministic chunk reconstruction,
per-block resource rejection, source-mutation invalidation, distinct
selection identities, atomic complete-source copying, partial-copy
rejection, and bounded large volume and 91k-grayordinate evidence.
