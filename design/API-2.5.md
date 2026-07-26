# neurogeo 2.5 API

- `write_ngeo_cifti()` writes CIFTI-2 dscalar, dlabel, and dtseries in pure R.
- `ngeo_delayed_values()` creates one callback/file-backed aligned values
  block; `ngeo_value_chunks()` iterates deterministic row chunks.
- `ngeo_block_support_map()`, `ngeo_validate_block_support_map()`,
  `ngeo_materialize_support_map()`, and `ngeo_change_support_block()` manage
  one logically hashed block sparse operator.
- `ngeo_bids_sidecar()` and `write_ngeo_bids_derivative()` write scoped
  derivative metadata and data, not a BIDS dataset manager.

The Matrix Market plus JSON support-map exchange remains schema-versioned and
backward compatible. All neurogeo 2.4 APIs remain available.
