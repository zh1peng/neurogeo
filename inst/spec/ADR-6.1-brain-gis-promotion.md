# ADR: promote the 6.1 brain-GIS analysis surface

Status: accepted for implementation, 2026-08-12.

## Decision

Version 6.1 adds a stable, analysis-only brain-GIS surface on the unchanged
NGCS 6.0 data model. The following ten entry points are approved as stable
exports:

- `ngeo_maup_sensitivity()`;
- `ngeo_local_layer_coupling()`;
- `ngeo_operator_graph()`;
- `ngeo_operator_path()`;
- `ngeo_wavelet_coupling()`;
- `ngeo_contiguous_regionalization()`;
- `ngeo_brain_landscape()`;
- `ngeo_resistance_distance()`;
- `ngeo_brain_point_process()`; and
- `ngeo_nonseparable_hotspots()`.

`print.ngeo_gis_analysis()` is approved as the shared registered S3 method.
Nine entry points return a method-specific class inheriting from
`ngeo_gis_analysis`; `ngeo_contiguous_regionalization()` returns the existing
stable `ngeo_partition` class. The lifecycle baseline therefore changes from
238 to 248 exports, from 98 to 99 registered S3 methods, and from 112 to 122
public classes. The eight registered generics are unchanged.

The existing `ngeo_spatial_basis()` API additionally accepts
`operator = "cotangent"` for metric-eligible surface coordinates and records a
cotangent stiffness operator with lumped barycentric mass. This is an additive
operator promotion, not a new export or result class; the graph-Laplacian path
remains available.

## Contract boundary

The files `api-lifecycle-6.0.csv`, `api-contracts-6.0.json`, and
`inference-contracts-6.0.csv` retain their names and remain the canonical 6.x
registries. The lifecycle registry records the ten exports, the S3 method, the
nine concrete result classes, and `ngeo_gis_analysis`. The inference registry
adds contracts for the seven scientific result classes:

- `ngeo_maup_sensitivity`;
- `ngeo_local_layer_coupling`;
- `ngeo_wavelet_coupling`;
- `ngeo_brain_landscape`;
- `ngeo_resistance_distance`;
- `ngeo_brain_point_process`; and
- `ngeo_nonseparable_hotspots`.

Operator graphs and paths describe support transformations rather than
scientific estimands. Regionalization returns an existing partition contract,
and a spatial basis is an analysis operator. These objects therefore do not
receive new inference-registry rows merely because they are stable.

## Scientific limitations

These are the accepted 6.1 limitations. The reviewed 6.2 amendments are
recorded in `ADR-6.2-brain-gis-hardening.md`; they do not retroactively change
the meaning of a result produced by a 6.1 release artifact.

- MAUP output is descriptive support sensitivity, not evidence of atlas
  invariance or a valid basis for choosing a support and testing on the same
  data.
- Local-layer permutation inference conditions on the observed maps, assumes
  free element exchangeability, and does not preserve spatial
  autocorrelation or provide population inference.
- Wavelet output is conditional on the fixed retained eigenbasis; reported
  energy and spectral-coverage diagnostics expose truncation.
- Landscape thresholds, resistance distances, and nonseparable hotspot states
  are descriptive. They carry no calibrated causal or sampling interpretation.
- Point-process simulations use the recorded finite discrete CSR reference.
  Their envelopes are pointwise, not familywise, and do not provide
  subject-level population inference.
- A data-derived regionalization must be frozen on training data before any
  downstream outcome is tested.

## Consequences

This decision amends the 6.0 lifecycle and inference-contract ADRs without
changing the NGCS 6.0 container. Release generation must occur after source
freeze: regenerate Rd and `NAMESPACE`, then the lifecycle registry, then the
API-contract snapshot. The lifecycle, feature-freeze, inference-contract, and
API-contract gates must all agree before release.

The existing complexity gate remains active. The four public entry points and
one basis validator that cross its high-risk threshold are recorded with their
frozen 6.1 line/branch counts and no growth allowance. This acknowledges their
validation-heavy facades without exempting later changes from refactoring or a
new reviewed ceiling decision.
