# Migration from neurogeo 5.x to NGCS 6.0

Version 6.0 changes both names and object contracts. Use
`ngeo_migrate_5x()` for complete in-memory 5.x objects; do not rename list
fields by hand. The converter preserves element and value order, map IDs,
measurement semantics, labels, and the original provenance. It also records an
explicit migration operation in 6.0 history.

```r
legacy <- readRDS("analysis-created-with-neurogeo-5.x.rds")
migrated <- ngeo_migrate_5x(legacy)

if (inherits(migrated, "ngeo_migration_report")) {
  print(migrated)
  # Re-read the source imaging file with a 6.x reader.
} else {
  ngeo_validate(migrated, "strict")
  saveRDS(migrated, "analysis-ngcs-6.0.rds")
}
```

## Support matrix

| 5.x object | Automated migration requirements | 6.0 object |
|---|---|---|
| `ngeo_points` | finite coordinates and ordered element table | `ngeo_point` |
| `ngeo_surface` | coordinates, coordinate metadata, faces, mask, and space | `ngeo_surface` |
| `ngeo_volume` | dimensions, affine, mask, and ordered voxel indices | `ngeo_volume` |
| `ngeo_regions` | region elements and support fields | `ngeo_parcellation` |
| `ngeo_grayordinates` | complete ordered surface/volume components | `ngeo_grayordinate` |
| delayed/file-backed values | not migrated; re-read source | structured reconstruction report |
| incomplete geometry, ambiguous measure references, unknown extensions | not migrated | structured reconstruction report |

The report has `status`, `source_schema`, `target_schema`, `base_type`,
`issues`, and `field_map`. Its `migrated` field is always `NULL`: the function
never returns a partially migrated object. The report does not contain a source
path or other private machine-local identifiers.

## Field and semantics mapping

| 5.x term | 6.0 term |
|---|---|
| `domain` | `base` |
| `maps` | `layers` |
| `map_id` | `layer_id` |
| `provenance` | `history$source_provenance` |
| `space` | `coordinate_space` |
| `space$units` | `coordinate_space$unit` |
| `measures$spatial_semantics` | `measures$support_behavior` |
| `measures$units` | `measures$unit` |
| `measures$default_aggregation` | `measures$aggregation` |
| `weights` | `spatial_weights` |
| `ngeo_bind_maps()` | `ngeo_bind_layers()` |
| `ngeo_change_support()` | `aggregate_to()` |
| public `map =` selector | `layer =` |
| BIDS `DomainType` / `DomainHash` | `BaseType` / `BaseHash` |
| QC `domain_type` / `map_summary` | `base_type` / `layer_summary` |

The 5.x measure table had one row per map. The converter deduplicates equal
semantic rows into stable 6.0 measures and makes each layer reference the
resulting immutable `measure_id`. It does not infer a physical unit, coordinate
role, registration, support behavior, missing geometry, or a replacement for
an unsupported value backend.

For crisp aggregation, create a partition and use `ngeo_aggregate()`, or build
an explicit support map and call `aggregate_to()`. Both execute the normative
aggregation engine. Arbitrary aggregation callbacks were removed so that the
result cannot bypass conservation, support weighting, and recorded measurement
semantics.

Portable object manifests are versioned as NGCS 6.0 and use `base_type`,
`base_sha256`, `layer_count`, and `ordered_layer_id_sha256`.

## Cross-atlas inference

`ngeo_cross_atlas_consensus()` no longer treats atlas-specific estimates as
independent by default. Calls without a covariance matrix now return a
descriptive marginal-precision-weighted estimate with `NA` confidence and
p-value fields. Use `covariance =` for covariance-aware fixed-effect GLS, or
set `independence = TRUE` to reproduce the earlier fixed/DerSimonian--Laird
analysis under an explicit independence assumption. A correlated random-effects
model is not silently approximated and is rejected.

## Sampling-unit declarations

Existing `ngeo_exchangeability()` calls remain subject-level by default. New
code should set `unit_kind = "subject"`, `"site"`, or `"spatial_block"`
explicitly when that distinction matters. Map-level spatial nulls are not an
exchangeability unit and now raise a typed error if passed as `map_null`.
