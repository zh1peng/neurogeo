# Migration to neurogeo 4.0

- Pass controlled metric names instead of metric objects.
- Pass sparse `ngeo_support_map` objects to `ngeo_change_support()`; remove
  block-support wrappers.
- Validate registered objects with `ngeo_validate()`.
- Read large values through format-specific file-backed readers and process
  them with `ngeo_value_chunks()`.
- Use `ngeo_gwr()` and `ngeo_kriging()` instead of batching wrappers.
- Use replay and artifact writers instead of generic execution, cache, or
  atomic-write helpers.
- Treat the namespace and migration guide as the API contract; schema and
  conformance catalogs are package verification details.
