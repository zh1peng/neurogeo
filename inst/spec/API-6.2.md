# neurogeo API 6.2

Version 6.2 is an additive minor release on the frozen NGCS 6.0
`base / values / layers / measures / history` model. The compatibility and
deprecation rules in `API-6.0.md` continue to apply. Version 6.2 changes no
core container field or spatial-base type.

## Stable additions

The following stable entry points extend the 6.1 brain-GIS surface:

| Entry point | Stable role | Return class |
|---|---|---|
| `ngeo_freeze_regionalization()` | Freeze a training-derived partition for independent application | `ngeo_frozen_regionalization` |
| `ngeo_apply_regionalization()` | Apply one frozen partition without relearning membership | `ngeo_partition` |
| `ngeo_hotspot_group_features()` | Convert independent-unit hotspot maps into group-test features | `ngeo_subject_features` |

`print.ngeo_frozen_regionalization()` is the registered display method for the
new frozen object. The lifecycle baseline is 251 exports, 100 registered S3
methods, 123 public classes, and eight registered generics.

The existing `ngeo_moran_null()` entry point is stable only for centered
singleton Moran spectral randomization. It preserves the input mean, centered
sum of squares, and Moran quadratic form to the registered numerical tolerance.
Other spatial-null methods remain governed by their explicit lifecycle and
calibration status.

## Revised stable scientific contracts

Version 6.2 revises three registered methods without changing the NGCS data
model:

- `ngeo_local_layer_coupling()` can use singleton Moran spectral
  randomization of the lagged map stack. This is reference-map inference, not
  independent-unit or population inference. Max-T can cover all requested
  local endpoints within an independent unit or one map-specific family.
- `ngeo_brain_point_process()` can construct simultaneous studentized global
  envelopes across the declared radii within each marked-pair family. These
  are finite-domain CSR simulation envelopes, not subject-level population
  intervals.
- `ngeo_nonseparable_hotspots()` can use one joint space-time reference-map
  action with max-T across all local cells in one independent unit.
  `ngeo_hotspot_group_features()` preserves independent units for a subsequent
  `ngeo_group_test()`; it does not reuse local reference-map p-values as group
  evidence.

The canonical rows are in `inference-contracts-6.0.csv`; the retained filename
denotes the complete 6.x registry. Stable API status describes the callable
contract and does not by itself establish operating characteristics outside
the executable suites registered in `validation-registry-6.0.csv`.

## Portable identity hardening

Portable object manifests use `NGCS-object-manifest-2`, and logical NGCS
object payloads use `NGCS-logical-object-2`. Schema 2 binds the selected
surface `base$geometry$active_coordinates`. Schema-1 object manifests are
rejected as unsupported and must not be reused for cache, checkpoint, or
artifact identity under 6.2.

Strict validation checks the selected surface coordinate set, coordinate-space
fields, measurement vocabulary, and structured dataset history. These checks
strengthen enforcement of existing NGCS 6.0 invariants; they do not add a new
container field.

## Evidence boundary

The software reports these procedures only within the estimand, null,
multiplicity family, support, and non-population boundaries recorded above.
Package validation establishes documented software behavior; study-specific
calibration and substantive claims belong with the consuming analysis.
