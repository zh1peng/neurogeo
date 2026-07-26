# Migrating from neurogeo 3.2 to 3.3

The 3.3 temporal layer is opt-in and additive. Construct an explicit
`ngeo_time_axis()` and attach it with `ngeo_set_time_axis()`; map count alone
never establishes temporal meaning.

Choose instantaneous or interval support before selecting temporal
measurement semantics. Interval totals cannot use the default temporal mean,
and instantaneous observations cannot use interval integration. These are
classed errors rather than implicit conversions.

Keep existing `ngeo_weights` as the spatial component. Add a sparse
`ngeo_temporal_weights`, then create a separable
`ngeo_spatiotemporal_weights`. Production lag and Moran execution remains
matrix-free. Only small independent-reference tests should materialize a
Kronecker operator.

`ngeo_time_slice()` preserves the exact spatial domain hash and selects only
the map/value/measure dimension. Longitudinal helpers return ordinary spatial
maps and retain source domain and time-axis identities in provenance.
