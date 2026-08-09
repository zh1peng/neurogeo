# Migration to 6.0

Version 6.0 replaces the 5.x core container and intentionally does not read
serialized 5.x `ngeo` objects.

| 5.x term | 6.0 term |
|---|---|
| `domain` | `base` |
| `maps` | `layers` |
| `map_id` | `layer_id` |
| `provenance` | `history` |
| `space` | `coordinate_space` |
| `weights` | `spatial_weights` |
| `ngeo_bind_maps()` | `ngeo_bind_layers()` |
| `ngeo_change_support()` | `aggregate_to()` |
| public `map =` selector | `layer =` |
| BIDS `DomainType` / `DomainHash` | `BaseType` / `BaseHash` |
| QC `domain_type` / `map_summary` | `base_type` / `layer_summary` |
| `ngeo_aggregate(fun, na.rm, tie)` | declare measure semantics and use `unknown`, `allocation`, and `unmapped` |

Re-read source imaging files with the 6.0 readers or reconstruct objects with a
6.0 constructor. Do not rename fields inside an old serialized list: geometry,
measure references, labels, hashes, and history contracts also changed.

For crisp aggregation, create a partition and use `ngeo_aggregate()`, or build
an explicit support map and call `aggregate_to()`. Both execute the same
normative aggregation engine. Arbitrary aggregation callbacks were removed so
that the result cannot bypass conservation, support weighting, and recorded
measurement semantics.

Portable object manifests are versioned as NGCS 6.0 and use `base_type`,
`base_sha256`, `layer_count`, and `ordered_layer_id_sha256`.
