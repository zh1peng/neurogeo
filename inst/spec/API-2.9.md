# neurogeo 2.9 API

## CIFTI contract

- `ngeo_validate_cifti_contract()` validates supported axes, label tables,
  NamedMap metadata, and datatypes.
- `write_ngeo_cifti()` accepts `datatype` and `named_map_metadata` for the
  supported pure-R dscalar, dlabel, and dtseries contracts.
- `read_ngeo_cifti()` exposes supported NamedMap metadata and the stored
  datatype in provenance.

## BIDS derivative transactions

- `ngeo_bids_parse_name()` parses one supported derivative filename.
- `ngeo_bids_build_name()` validates and canonically orders entities.
- `ngeo_validate_bids_sidecar()` verifies domain, space, semantics,
  provenance, and entity bindings.
- `write_ngeo_bids_derivative()` supports strict names and explicit
  `error`, `overwrite`, or `version` collision handling while promoting the
  data-sidecar pair atomically.

## Support-map exchange schema 2

- `write_ngeo_support_bundle()` writes ordered Matrix Market chunks and a
  checksummed JSON manifest.
- `ngeo_validate_support_bundle()` validates structure, ordering, sizes, and
  SHA-256 checksums.
- `read_ngeo_support_bundle()` verifies and reconstructs the logical map.
- `ngeo_migrate_support_map_exchange()` migrates schema 1 or rewrites schema
  2 without changing the logical hash.
- `read_ngeo_support_map()` also recognizes schema-2 bundle paths.

## Conformance and 3.0 readiness

- `ngeo_conformance_manifest()` verifies the NGCS 1.0-2.9 JSON corpus.
- `ngeo_compatibility_matrix()` separates local validation evidence from
  configured remote CI.
- `ngeo_api_inventory()` records every public export and 3.0 disposition.

All neurogeo 2.8 APIs remain available. No 2.x export is deprecated.
