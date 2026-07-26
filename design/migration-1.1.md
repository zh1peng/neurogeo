# Migrating from neurogeo 1.0 to 1.1

neurogeo 1.1 is backward compatible with the public 1.0 analysis API.

- Existing `permutations`, `seed`, and `alternative` arguments remain valid.
- `ngeo_permutation_control()` can now supply a shared permutation, tail,
  seed, and multiple-testing policy. When `control` is supplied, its fields
  override the corresponding scalar arguments.
- Local Moran results add `p.adjusted`; existing columns retain their names
  and alignment.
- Surface charts are auxiliary, explicitly non-metric coordinates. They
  never replace active anatomical coordinates.
- `ngeo_as_sf()` is a one-way interoperability copy and requires an explicit
  chart for surfaces.
