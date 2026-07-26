# Migrating from neurogeo 3.2 to 3.3

Version 3.3 is additive. Existing datasets remain non-temporal until an
explicit `ngeo_time_axis` is attached. No existing map count or format
metadata is silently reinterpreted as time.

Create a time axis with coordinates and units. For interval measurements also
supply start/end support and select `interval_mean`, `interval_total`, or
`rate` when binding the axis. Use `instantaneous` only with instantaneous
support and `categorical` for labels.

Build spatial and temporal weights independently, then combine them with
`ngeo_spatiotemporal_weights()`. Use matrix-free lag and Moran methods for
normal execution. Full Kronecker materialization exists only for small
reference checks and requires explicit limits.

Time slicing preserves the original spatial domain identity. Derived change,
trend, and contrast results intentionally remove the time axis and produce
one or more ordinary spatial maps with source domain/axis provenance.
