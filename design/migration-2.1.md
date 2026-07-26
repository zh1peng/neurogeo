# Migrating from neurogeo 2.0 to 2.1

Version 2.1 is additive. Existing 2.0 constructors and analyses do not need
to change.

## Prefer builders for real relationships

```r
map <- ngeo_surface_registration_map(
  source,
  target,
  method = "barycentric",
  source_coordinates = "sphere",
  target_coordinates = "sphere",
  registration = "fs_LR"
)

atlas <- ngeo_atlas_map(source, labels)
regional <- ngeo_change_support(source, atlas$target, atlas)
```

## Validation is stricter

`ngeo_validate_support_map()` now checks the fixed direction, scalar domain
identities, ordered character element IDs, provenance type, valid sparse
slots, tolerance, and consistency of stored target support with
`operator %*% source_support`.

Objects produced by public 2.0 constructors already satisfy these checks.
Hand-edited or externally deserialized objects should be reconstructed
through `ngeo_support_map()` or `read_ngeo_support_map()`.

## Registration remains external

The new surface and volume functions consume known coordinate relationships.
They do not call registration software and do not make different spaces
compatible merely because coordinates are numerically close.

## Inference claims

Use `ngeo_parcellation_inference()` only for the two global invariants
defined by NGCS 2.0. Use `ngeo_atlas_robust_effect()` to report how an effect
changes across atlases; do not relabel that result as parcellation invariant.
