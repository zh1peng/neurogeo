# Migration to neurogeo 4.6

Version 4.6 does not change the NGCS core object, layer index, or fixed basis.
Existing 4.5 workflows remain valid.

Use `ngeo_layer_coupling()` after `ngeo_validate_layers()`. Supply a fixed
`ngeo_spatial_basis` for spectral/energy/roughness endpoints and explicit
`ngeo_weights` for directional lag or classic cross-Moran. More than two
layers require an explicit pair table or `pairs = "all"`.

Spectral coupling columns no longer stand alone: layer energy, cross-energy,
retained variance, and low-energy diagnostics are part of the same result.
The classic Moran compatibility statistic and support-weighted lag
correlation use different formulas and endpoint names.

An optional `null` list is only for one-unit reference-map analysis. It must
declare randomized and fixed stacks plus one shared `ngeo_null`
transformation group. Its p-values are not population p-values. Subject-level
permutation inference is introduced separately in 4.7.
