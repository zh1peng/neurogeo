# neurogeo 3.3 development target

## Goal

Add explicit temporal and spatiotemporal semantics while preserving one
spatial domain and one element-by-map values block. Time aligns to maps; no
vertex/voxel geometry may be duplicated for each time point.

## Modules

1. A regular/irregular `ngeo_time_axis` with stable identity, canonical unit,
   instant or interval support, deterministic slicing, and exact map binding.
2. Temporal neighbor/weight objects and separable spatiotemporal weights that
   retain one spatial sparse operator and one temporal sparse operator.
   Matrix-free lag is normative; explicit Kronecker materialization is a
   guarded small-reference operation.
3. Temporal and spatiotemporal Moran statistics plus bounded temporal and
   joint space-time variograms with deterministic pair accounting.
4. Longitudinal change, per-element trend, and temporal contrast helpers whose
   validity and weighting follow instantaneous, interval-mean, interval-total,
   rate, or categorical semantics.
5. NGCS schemas/corpus, migration/API/specification documents, tutorials,
   exact/property/adversarial tests, large matrix-free validation, and release
   evidence.

## Exit criteria

- Regular and irregular axes, instant/interval support, mutation detection,
  and deterministic index/range slicing pass exact references.
- Separable lag and Moran agree with explicitly materialized small Kronecker
  matrices, while a large gate stores only component operators and stays
  within resource limits.
- Temporal and space-time variograms report exact pair counts and reject
  requests beyond their pair budget.
- Change, trend, mean/sum/integral contrasts accept only compatible temporal
  semantics and duration support.
- No object expands spatial geometry across time and no API silently infers a
  time axis from map count alone.
- neurogeo 3.3.0 archive, reports, SHA-256, documentation, and
  `R CMD check --as-cran: Status OK` are complete before 3.4 begins.
