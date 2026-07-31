# neurogeo API 5.0 freeze

Version 5.0 freezes the seven public entry points added for the multilayer
workflow. Existing pre-4.5 APIs remain available; this list is an upper bound
on the new stable multilayer surface, not on the package's historical exports.

| Entry point | Stable role | Return contract |
|---|---|---|
| `ngeo_bind_maps()` | Bind exactly aligned map columns | ordinary `ngeo` |
| `ngeo_validate_layers()` | Index unique unit-layer combinations | `ngeo_layer_index` |
| `ngeo_spatial_basis()` | Build a value-independent topology basis | `ngeo_spatial_basis` |
| `ngeo_basis_project()` | Project maps with support weighting | `ngeo_subject_features` |
| `ngeo_layer_coupling()` | Compute declared layer estimands | `ngeo_subject_features` |
| `ngeo_exchangeability()` | Freeze a subject transformation schedule | `ngeo_exchangeability` |
| `ngeo_group_test()` | Run subject-level or support-family inference | `ngeo_group_result` |

`ngeo_spatial_features()` is not exported. The narrower projection and
coupling functions cover its proposed role without another facade.

The 4.9 functions `ngeo_spatial_ordination()`, `ngeo_coregionalization()`, and
`ngeo_mgwr()` remain experimental. They are not part of this stable surface.

## Naming and schema rules

- `unit_id` is the canonical independent-unit identity.
- `map_id` and `endpoint_id` are ordered, unique identities, not display
  labels.
- `support_hash`, `basis_hash`, `schedule_hash`, and `family_hash` identify
  the corresponding immutable analysis objects.
- Arguments named `missing`, `components`, `conflicts`, `storage`,
  `adjustment`, and `transform` use explicit enumerated policies.
- Results retain values separately from unit/endpoint metadata and diagnostics.
- Subject-feature rows are complete independent units; elements and maps are
  never treated as replicated subjects.

## Conditions

All package errors inherit from `ngeo_error`. The stable multilayer path uses
specific subclasses for argument, alignment, domain, measurement, support,
resource, exchangeability, design, test-term, and statistic failures. Tests
assert subclasses for the high-risk scientific boundaries; callers should
normally catch `ngeo_error` unless recovery depends on a specific boundary.

## Compatibility promise

Within the 5.x series, removal or incompatible renaming of these seven
functions, their formal arguments, primary result classes, or identity fields
requires a documented deprecation cycle. Adding optional output diagnostics is
allowed when existing fields keep their meaning.
