# neurogeo 4.2.1 public API tiers

The stable core covers five domain constructors, aligned value access,
space/measurement semantics, topology and weights, partitions and support
change, foundational statistics/models, standard format I/O, validation,
identity, provenance, and the 4.3 atlas-independent cortical cartography
entry points `ngeo_flatten_surface()`, `ngeo_project_surface()`,
`ngeo_cortical_map()`, `ngeo_cortical_map_data()`, and
`ngeo_cortical_layout()`.

Advanced scientific functions cover explicit nulls, uncertainty,
resampling, time/space-time analysis, iterative models, transform graphs,
streaming, and file-backed execution.

Exchange/governance functions cover format contracts, portable manifests,
BIDS derivatives, replay, artifact batches, support exchange, and optional
ecosystem conversion.

The tiers are navigation aids. All exported functions retain their existing
lifecycle status.
