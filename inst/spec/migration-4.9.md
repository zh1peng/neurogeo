# Migration to 4.9

Version 4.9 is additive. Existing 4.8 workflows and object schemas are
unchanged.

The new ordination, coregionalization, and MGWR functions are experimental.
Code should inspect each result's `status`, inference declaration, assumptions,
and promotion blockers. Do not interpret reference-map ordination or MGWR
local coefficients as population-level hypothesis tests.

For confirmatory multilayer analysis, continue to use the fixed-basis path:
`ngeo_spatial_basis()` -> `ngeo_layer_coupling()` -> `ngeo_group_test()`.
