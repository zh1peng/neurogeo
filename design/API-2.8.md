# neurogeo 2.8 API

## Space registry and audit

- `ngeo_space_hash()` computes the stable normative space identity.
- `ngeo_space_registry()`, `ngeo_register_space()`,
  `ngeo_validate_space_registry()`, and `ngeo_resolve_space()` manage exact
  spaces and aliases.
- `ngeo_space_audit()` compares kind, units, dimensionality, structure,
  template, density, and resolution.

## Transform graph

- `ngeo_transform_hash()` hashes one supplied transform.
- `ngeo_transform_graph()`, `ngeo_add_transform()`, and
  `ngeo_validate_transform_graph()` manage immutable directed edges.
- `ngeo_transform_path()` returns one unique or caller-selected shortest path.
- `ngeo_transform_graph_diagnostics()` reports cycles, ambiguous pairs, and
  edge space mismatches.
- `ngeo_transform_path_provenance()` exports every traversed edge and hash.
- `ngeo_apply_transform_path()` requires `authorize = TRUE` and applies only
  a non-lossy affine path.

All neurogeo 2.7 APIs remain available.
