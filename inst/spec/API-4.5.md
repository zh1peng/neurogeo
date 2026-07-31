# neurogeo 4.5 API contract

Version 4.5.0 adds four stable entry points. It does not change the NGCS 3.5
core object schema.

## Layer indexing

`ngeo_validate_layers()` interprets existing map-table columns as one logical
independent-unit by layer index. It requires unique unit-layer combinations,
reports whole-map availability, optionally requires a complete declared layer
family, and validates one measurement contract per layer. It never reads the
values block.

## Exact map binding

`ngeo_bind_maps()` binds map columns only when every input has the same exact
ordered domain, element IDs, space hash, domain type, and topology identity.
It performs no registration or resampling. In-memory binding is resource
guarded. If any source is delayed/file-backed, automatic storage constructs a
composite delayed callback and preserves the original source-reader mutation
checks.

## Fixed graph basis

`ngeo_spatial_basis()` uses symmetric, finite, non-negative raw weights. For
each connected component it solves the generalized graph-Laplacian problem
with one positive support mass and returns only requested non-constant modes.
Large paths use a sparse partial eigensolver and never construct a dense
full-domain matrix. Operator, support, basis, domain, and space identities plus
residual, weighted orthogonality, nullity, and degeneracy diagnostics are
retained.

The stable 4.5 operator is `graph_laplacian`. Cotangent Laplace-Beltrami is not
part of this contract.

## Projection

`ngeo_basis_project()` reads component-row and map-column chunks and returns
one `ngeo_subject_features` endpoint matrix. Projection requires continuous
intensive maps on the exact basis domain. Coefficients, absolute/relative band
energy, retained variance, residual energy, and normalized roughness use the
same support-weighted centering and inner product. Bands cannot overlap or
split a near-degenerate eigenspace.

This version provides no layer-coupling p-value and no population inference.
