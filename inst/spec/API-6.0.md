# neurogeo API 6.0

Version 6.0 is the breaking boundary that introduces the
`base / values / layers / measures / history` model. Within the 6.x series,
removal or incompatible renaming of stable entry points requires a documented
deprecation cycle.

## API tiers

### Core stable

- constructors: `ngeo_point()`, `ngeo_surface()`, `ngeo_volume()`,
  `ngeo_parcellation()`, and `ngeo_grayordinate()`;
- accessors: `spatial_base()`, `values()`, `layers()`, `measures()`, and
  `history()`;
- validation and identity: `ngeo_validate()`, `ngeo_capabilities()`,
  `base_type()`, and `base_hash()`;
- aligned data operations: `ngeo_subset()` and `ngeo_bind_layers()`;
- explicit spatial operations: `ngeo_distance()`, `ngeo_spatial_weights()`,
  `ngeo_support_map()`, and `aggregate_to()`;
- generic I/O: `read_ngeo()` and `write_ngeo()`.

The preferred accessors are `ngeo_spatial_base()`, `ngeo_base_elements()`,
`ngeo_values()`, `ngeo_layers()`, `ngeo_measures()`, `ngeo_history()`,
`ngeo_base_type()`, and `ngeo_base_hash()`. Their unprefixed predecessors remain
compatible for all 6.x releases. `ngeo_layer_index()` is the accurately named
feature-index constructor; `ngeo_validate_layers()` remains a compatibility
entry point.

### Stable analysis

Documented statistics, modelling, resampling, temporal, reproducibility, and
inference functions are stable when their help page does not mark them
experimental. They consume core objects but do not extend the core container.

Stable scientific result classes are covered by
`inference-contracts-6.0.csv`. Use `ngeo_inference_contract()` to inspect the
estimand, sampling unit, null model, metric, support, and uncertainty target.
The returned contract has one canonical representation shared by its print,
summary, and portable object manifest. Experimental result classes do not
receive a stable inference contract by implication.

### Experimental

`ngeo_spatial_ordination()`, `ngeo_coregionalization()`, and `ngeo_mgwr()` are
experimental. Their result fields and numerical limits may change between
minor releases. They must not be presented as part of the core stable surface.

### Contract validators

Specialized `ngeo_validate_*()` functions validate portable analysis-object
contracts. `ngeo_validate()` is the preferred user entry point.
`ngeo_validate_layers()` is retained as a 6.x compatibility entry point for
`ngeo_layer_index()`.

## Naming rules

- `base` describes spatial identity; `domain` is not a current container term.
- `layer` selects an aligned values column.
- `support_map` describes a source-to-target mapping and remains a valid use of
  “map”.
- `spatial_weights` names neighborhood weights.
- `history` names operation records; `provenance` may appear only in historical
  specifications or general explanatory prose.

## Superseded convenience entry points

`ngeo_aggregate()` is a convenience wrapper around the normative
partition-to-support-map-to-`aggregate_to()` path. It must not contain a second
aggregation implementation. `ngeo_spatial_regression()` is the general model
entry point; `ngeo_spatial_lm()` remains the narrower OLS/SLX convenience API.

## 6.0 argument changes

The single-column selector formerly named `map` is named `layer` throughout
the public API. Collections of columns continue to use `layers`. BIDS-style
sidecars use `BaseType`, `BaseHash`, and `History`; QC and exported history
records use `base_type` and layer-oriented summaries.
