# Migrating from neurogeo 2.8 to 2.9

Version 2.9 is additive. Existing CIFTI calls, BIDS derivative calls, and
schema-1 support-map exchange remain available.

CIFTI writes now validate the complete supported axis contract. Existing
dscalar and dtseries outputs default to float32; dlabel defaults to and
requires int32. Pass `datatype = "float64"` when scalar precision must be
retained. Attach `named_map_metadata` only to dscalar or dlabel named maps;
series metadata belongs to the time-axis fields already stored in `maps`.

For BIDS derivatives, prefer `ngeo_bids_build_name()` and
`strict_name = TRUE`. Select the collision policy explicitly. The default
continues to reject an existing output; `collision = "version"` adds the
next deterministic run entity. Data and JSON are now committed together.

Schema-1 support exchange remains readable. Use
`ngeo_migrate_support_map_exchange(old, new)` to create a schema-2 directory
bundle. Consumers should call `ngeo_validate_support_bundle()` before
archival transfer; `read_ngeo_support_bundle()` always verifies checksums
and the complete logical map hash.

Use `ngeo_api_inventory()` when preparing 3.0 integrations. The 2.9
inventory marks every export stable and retained, so no migration is
required solely for deprecation.
