# Neuroimaging Geoinformatics Core Specification 2.5 addendum

Status: stable  
Version: 2.5  
Base specification: NGCS 2.4

NGCS 2.5 standardizes scalable values, block sparse support, pure-R CIFTI
output, and scoped BIDS derivatives. All NGCS 1.0-2.4 contracts remain.

A delayed values object is one logical element-by-map block. It MUST expose
fixed dimensions, ordered map names, auditable backing identity, aligned
two-dimensional subsetting, and deterministic chunk iteration. It MUST NOT
be presented as a multi-assay container.

A block support map partitions one logical target-by-source sparse operator.
Row and column groups MUST be complete, disjoint, ordered, and hash-equivalent
to monolithic materialization. Partitioning MUST NOT change element order,
orientation, semantics, or uncertainty.

Pure-R CIFTI writing MUST preserve CIFTI-2 brain-model order, structures,
surface vertex indices/counts, voxel IJK indices, affine, values, map names,
label keys, and series metadata for dscalar, dlabel, and dtseries. Workbench,
FSL, and FreeSurfer MUST NOT be runtime dependencies.

BIDS support writes explicitly requested derivatives and JSON sidecars with
spatial entities, measurement semantics, sources, and provenance. It MUST
NOT claim dataset indexing, validation, or orchestration.

Conformance requires three CIFTI golden write/read/write families, delayed
versus in-memory equality, block versus monolithic operator/value/hash
equality, schema-1 support-map backward compatibility, BIDS sidecar fixtures,
and a one-million-source sparse/delayed resource gate.
