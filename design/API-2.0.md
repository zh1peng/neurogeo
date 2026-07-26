# neurogeo 2.0 API

## Support-map core

- `ngeo_support_map(source, target, operator, type, ...)`
- `ngeo_validate_support_map(x)`
- `ngeo_support_map_hash(x)`
- `ngeo_support_map_from_partition(source, partition, target)`
- `ngeo_compose_support_map(first, second)`

Every operator is sparse `target × source` and bound to ordered source and
target domain hashes.

## Change of support and uncertainty

- `ngeo_change_support(x, target, support_map, ...)`
- `ngeo_support_variance(x, target, support_map, value_variance, ...)`

The former returns one ordinary NGCS dataset on the target domain with one
aligned values block. Uncertainty remains a separate aligned matrix rather
than becoming an implicit second assay.

## Atlas operations

- `ngeo_atlas_overlap(first, second, metric)`
- `ngeo_atlas_compare(first, second)`
- `ngeo_cross_atlas(values, first, second, model)`
- `ngeo_parcellation_inference(x, support_maps, targets, ...)`

Cross-atlas transfer exposes a sparse transfer operator and requires the
`piecewise_constant` model. Parcellation-invariant inference is limited to
global support-weighted intensive means and extensive/count totals.

## Compatibility

All NGCS 1.0/1.x domain, reader, writer, transform, topology, weights,
partition, statistics, null, and model APIs remain available. A
`ngeo_partition` can migrate through `ngeo_support_map_from_partition()`.
