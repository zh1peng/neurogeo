# Neuroimaging Geoinformatics Core Specification 3.3

Status: stable  
Version: 3.3  
Base specification: NGCS 3.2

NGCS 3.3 aligns explicit time coordinates to maps while retaining exactly one
spatial domain and one element-by-map values block. Spatial geometry MUST NOT
be duplicated through time, and time MUST NOT be inferred from map count.

A time axis contains finite strictly increasing coordinates, one canonical
unit, instant or positive-width interval support, an overlap policy,
regularity metadata, tolerance, and immutable SHA-256 identity. Time-aware
maps declare `instantaneous`, `interval_mean`, `interval_total`, `rate`, or
`categorical` semantics compatible with that support. Slicing preserves the
exact spatial domain, ordered alignment, semantics, and provenance.

Temporal weights are non-negative zero-diagonal sparse operators bound to an
axis identity. Spatiotemporal weights retain one domain-bound spatial
operator and one axis-bound temporal operator. Matrix-free separable
Kronecker-sum/product execution is normative; full materialization is an
explicitly limited small-reference operation.

Temporal and spatiotemporal Moran statistics use the bound operators.
Temporal and joint semivariograms declare a hard pair budget and report exact
pair counts. Longitudinal change, trend, and contrasts preserve the spatial
domain and obey instant/interval measurement semantics and durations.
