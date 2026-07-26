# Migrating from neurogeo 2.7 to 2.8

Version 2.8 is additive. Existing single-transform construction, composition,
inversion, and affine geometry application remain available.

Use a registry when more than one coordinate-space description participates
in a workflow. Register the complete `ngeo_space`, then resolve it by exact
hash or explicit alias. A duplicate `space_id` is allowed to expose
conflicting metadata, but resolving that name is intentionally ambiguous.

Add only transforms already supplied by the caller or an external source.
Declare whether each edge is invertible and lossy. Path search never estimates
registration. When two shortest paths exist, pass the exact `selection`
sequence rather than relying on edge insertion order.

Inspect graph diagnostics and path provenance before application. Application
requires `authorize = TRUE` and remains geometry-only; lossy and non-affine
paths cannot be applied by neurogeo.
