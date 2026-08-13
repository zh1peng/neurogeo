# neurogeo API 6.1

Version 6.1 is an additive minor release on the NGCS 6.0
`base / values / layers / measures / history` model. The compatibility and
deprecation rules in `API-6.0.md` continue to apply.

## Stable brain-GIS analysis surface

| Entry point | Stable role | Return class |
|---|---|---|
| `ngeo_maup_sensitivity()` | Compare one association across declared scale and zoning supports | `ngeo_maup_sensitivity` |
| `ngeo_local_layer_coupling()` | Map directed local association between aligned layers | `ngeo_local_layer_coupling` |
| `ngeo_operator_graph()` | Register directed support maps between named spatial bases | `ngeo_operator_graph` |
| `ngeo_operator_path()` | Validate and compose one explicit support-operator path | `ngeo_operator_path` |
| `ngeo_wavelet_coupling()` | Compute localized cross-layer spectral coupling over declared scales | `ngeo_wavelet_coupling` |
| `ngeo_contiguous_regionalization()` | Learn a constrained contiguous MST partition | `ngeo_partition` |
| `ngeo_brain_landscape()` | Summarize thresholded patches and graph boundaries | `ngeo_brain_landscape` |
| `ngeo_resistance_distance()` | Compute distances conditioned on a declared conductance or barrier field | `ngeo_resistance_distance` |
| `ngeo_brain_point_process()` | Compare event-pair concentration with a finite discrete CSR reference | `ngeo_brain_point_process` |
| `ngeo_nonseparable_hotspots()` | Summarize exploratory graph-time hotspot states | `ngeo_nonseparable_hotspots` |

The nine method-specific analysis classes inherit from `ngeo_gis_analysis` and
share `print.ngeo_gis_analysis()`. Printed summaries are concise views of the
stored result; they do not add inferential meaning.

## Cotangent spatial basis

`ngeo_spatial_basis(operator = "cotangent")` is stable for a surface whose
coordinate role and unit establish metric eligibility. It uses the surface
triangles to build a symmetric cotangent stiffness operator and a lumped
barycentric-area mass vector. Degenerate geometry is rejected. The result
records its operator and metric contract; graph-Laplacian bases retain their
existing semantics.

For a cotangent basis, wavelet scale is operator diffusion time with squared
coordinate units. Graph-Laplacian scales are operator-specific and
dimensionless unless separately calibrated.

## Interpretation and inference

The stable result contracts are normative in
`inference-contracts-6.0.csv`; the retained filename denotes the canonical 6.x
registry. The statements below describe the 6.1 contract; `API-6.2.md` records
the later null-model and multiplicity amendments. In 6.1:

- MAUP sensitivity, landscape geometry, resistance distance, wavelet
  scale-space, and nonseparable hotspot states are descriptive under their
  recorded support, metric, thresholds, or operator;
- local-layer permutation p-values assume the recorded free element
  exchangeability and are adjusted only within the declared map when max-T is
  requested; and
- brain point-process envelopes are pointwise under the recorded finite
  discrete CSR simulation, not familywise or population-level intervals.

Operator graphs and paths preserve provenance for cross-base transformations;
they do not make a composed mapping anatomically correct. Regionalization is
analytic rather than anatomical segmentation, and a learned partition must be
frozen before independent downstream testing.

## Governance

`ADR-6.1-brain-gis-promotion.md` records the approved surface and count change.
`api-lifecycle-6.0.csv`, `api-contracts-6.0.json`, and
`inference-contracts-6.0.csv` remain the installed 6.x lifecycle, callable
contract, and scientific interpretation authorities respectively.
