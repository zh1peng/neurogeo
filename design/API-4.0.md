# neurogeo API 4.0

Version 4.0 is a smaller, science-facing public API. It changes package
architecture without changing the stable NGCS 3.5 scientific schemas or
numerical definitions.

## Retained contract

- one spatial domain and one strictly aligned values block;
- explicit space, topology, metric name, measurement semantics, transforms,
  and auditable provenance;
- surface, volume, points, grayordinates, and regions domains;
- sparse weights, partitions, support maps, and bounded materialization;
- format-specific NIfTI, GIFTI, CIFTI, FreeSurfer, BIDS-derivative, and
  portable support-map I/O;
- spatial statistics, support-aware inference, resampling, temporal
  analysis, spatial models, uncertainty, and whitelist-only replay.

`ngeo_validate()` is the public validation entry point for registered NGCS
objects. Format boundary validators remain separate where they validate a
file or exchange contract rather than an NGCS runtime object.

## Removed implementation API

The following 3.6 compatibility APIs are no longer exported:

- metric objects and public delayed-value constructors;
- block-support map wrappers and block-specific change-of-support methods;
- generic execution plans, execution caches, and atomic-write helpers;
- separate GWR and kriging batching wrappers;
- schema registries, validation reports, attribute-only schema migration,
  lifecycle tables, API inventories, compatibility matrices, and
  conformance-corpus readers.

Internal delayed readers and atomic publication still support bounded I/O.
They are implementation mechanisms, not additional scientific data models.

## Version policy

The package version and scientific specification version are independent.
neurogeo 4.0.0 implements the stable NGCS 3.5 schemas. A package API release
does not invent a new scientific schema when no scientific invariant
changed.
