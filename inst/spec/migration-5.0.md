# Migration to 5.0

Version 5.0 freezes and validates the 4.5--4.8 multilayer workflow. It is
additive relative to 4.4.2 and does not replace historical spatial-statistics
or support APIs.

Use the stable path:

```r
stack <- ngeo_bind_maps(subject_objects, metadata = map_metadata)
index <- ngeo_validate_layers(stack)
basis <- ngeo_spatial_basis(stack, weights = weights)
features <- ngeo_layer_coupling(stack, index, basis = basis)
schedule <- ngeo_exchangeability(subject_data$unit_id)
result <- ngeo_group_test(features, subject_data, ~ group, "group", schedule)
```

For several atlases or supports, pass one named list of compatible
`ngeo_subject_features` objects to the same `ngeo_group_test()` call. Do not
run separate schedules and combine their p-values afterward.

## Required changes in older exploratory code

- Replace vertex/region rows used as replicates with one row per independent
  subject.
- Replace data-derived confirmatory bases with topology-derived bases, or
  declare independent training for exploratory ordination.
- Label rank-matched scales as rank-matched; do not call them equal physical
  wavelengths.
- Keep `ngeo_spatial_ordination()`, `ngeo_coregionalization()`, and
  `ngeo_mgwr()` behind their experimental status and diagnostics.

No PR, tag, binary data redistribution, or package-specific file format is
introduced by the 5.0 migration.
