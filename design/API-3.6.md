# neurogeo API 3.6

Version 3.6 is the compatibility transition to the smaller 4.0 public API.
It does not change the NGCS 3.5 scientific model.

## Retained contracts

- one spatial domain and one strictly aligned values block;
- explicit space, topology, metric name, measurement semantics, transforms,
  and provenance;
- the five base domains;
- sparse weights, partitions, support maps, bounded distance execution, and
  format-specific I/O;
- spatial statistics, support-aware inference, resampling, temporal analysis,
  and scientifically distinct model families.

## Deprecated implementation APIs

The following implementation layers remain callable in 3.6 but are scheduled
for removal from the public namespace in 4.0:

- metric objects (`ngeo_metric()`); pass a declared metric name instead;
- delayed-value construction and the block-support class;
- generic execution plans, caches, and public atomic-write helpers;
- separate GWR and kriging batching wrappers;
- schema registry/introspection and the attribute-only schema migration;
- compatibility/conformance helpers intended for package tooling.

`ngeo_change_support()` accepts legacy block maps during the transition.
`ngeo_validate()` is the common validation entry point for registered objects.

The authoritative machine-readable lifecycle table is returned by
`ngeo_api_lifecycle()`.
