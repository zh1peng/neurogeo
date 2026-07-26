# Migrating from neurogeo 2.4 to 2.5

Version 2.5 is additive. In-memory values and monolithic support maps remain
the defaults.

Use `ngeo_delayed_values(reader, dim, map_names)` when values should be read
by aligned row/map slices, then `ngeo_value_chunks()` for bounded processing.
Use `ngeo_block_support_map()` to partition an existing validated logical
operator; its logical hash remains the monolithic support-map hash.

`write_ngeo_cifti()` is the pure-R writer boundary for dscalar, dlabel, and
dtseries. `write_ngeo_bids_derivative()` writes only the explicitly named
derivative and sidecar and does not orchestrate a dataset.
