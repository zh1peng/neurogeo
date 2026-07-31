# Migration to neurogeo 4.5

The core `ngeo` object and all 4.4 APIs remain unchanged.

Multilayer data continue to use ordinary map columns. Add `subject_id` and
`feature` columns to `ngeo_maps(x)`, then call `ngeo_validate_layers()`. Do not
create a separate cohort or tensor object.

Use `ngeo_bind_maps()` only for objects already sharing the exact ordered
domain. Existing resampling and change-of-support APIs remain the explicit way
to place data on another legal domain.

Graph bases require symmetric non-negative raw weights. A row-standardized
`ngeo_weights` object is accepted only because its separately retained
`raw_matrix` defines the basis; the row-standardized matrix is not
eigendecomposed.

`RSpectra` is suggested for large partial bases. Small reference cases retain
a bounded base-R eigensystem. No existing workflow acquires a new runtime
dependency unless it requests a large basis.
