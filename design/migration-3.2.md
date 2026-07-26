# Migrating from neurogeo 3.1 to 3.2

Version 3.2 is additive. Existing transform graph, support-map builder, and
change-of-support calls retain their behavior.

Use `ngeo_resampling_plan()` when a workflow needs one auditable contract
linking those existing pieces. First register exact spaces and supplied
transforms, resolve ambiguity with `ngeo_transform_path()`, then construct a
plan. Plan creation is inert. Both map construction and execution require
`authorize = TRUE`.

The plan's `coverage` policy concerns geometric contributions;
`missing` concerns wholly unmapped source elements; and `conservation`
controls extensive/count allocation. These are deliberately separate.
Intensive normalization continues to follow measurement semantics.

Set `uncertainty = "value"` only when aligned source value variance will be
provided. Use `"value_and_mapping"` only with both value variance and a
target-by-source weight-variance matrix. Variance remains a separate aligned
result component, not a second values block in the dataset.

Non-affine warp paths are retained by the transform graph for audit but are
not executable by the 3.2 bridge. Supply a supported affine path or build a
support map externally and validate it; neurogeo will not call external
registration/resampling software.
