# Migrating from neurogeo 2.5 to 2.6

Version 2.6 is additive. Existing in-memory values, monolithic support maps,
and modelling functions remain valid.

Use `ngeo_change_support_block()` when an existing `ngeo_block_support_map`
must remain blockwise throughout execution. Use `ngeo_block_diagnostics()`
and `ngeo_block_variance()` for bounded diagnostics and independent variance
propagation. Materialize only when a legacy consumer explicitly requires a
monolithic map.

Use `ngeo_stream_*()` functions for delayed-native summaries, covariance,
OLS, and Moran's I. These functions do not introduce another assay or values
container.

For resumable work, declare a resource budget, create an execution plan with
a complete identity, and pass a checkpoint path. Changing that identity
requires a new checkpoint. Use `ngeo_atomic_write()` for final artifacts and
`ngeo_cache_compute()` only with identities that include all
provenance-relevant inputs.
