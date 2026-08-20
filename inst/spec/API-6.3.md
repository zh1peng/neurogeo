# neurogeo API 6.3

Version 6.3 is an additive interoperability-infrastructure release on the
frozen NGCS 6.0 `base / values / layers / measures / history` model. The
compatibility and deprecation rules in `API-6.0.md` continue to apply. Version
6.3 changes no `ngeo` top-level field and adds no spatial-base type.

## Stable additions

| Entry point | Stable role | Return class |
|---|---|---|
| `ngeo_relation()` | Construct optional empirical pairwise information bound to one ordered Base | `ngeo_relation` |
| `ngeo_validate_relation()` | Validate a Relation and optionally prove Base alignment | `ngeo_relation` invisibly |
| `ngeo_layer_view()` | Extract one complete Base + Values + Measure + LayerMetadata spatial field | `ngeo_layer_view` |
| `base_signature()` | Compute a canonical cross-language SHA-256 Base identity | character |
| `ngeo_base_signature()` | Preferred prefixed alias of `base_signature()` | character |

`print.ngeo_relation()` and `print.ngeo_layer_view()` are the registered
display methods for the two new objects. The lifecycle baseline is 256
exports, 102 registered S3 methods, 125 public classes, and eight registered
generics.

## Relation contract

An `ngeo_relation` is independent of an `ngeo` dataset and has these stable
fields:

```text
base / data / type / directed / weighted / measure / provenance
```

`base` is a compact binding containing the R implementation hash, portable
SHA-256 signature, base type, and ordered element IDs. It does not duplicate
the full Base. `data` is either a square numeric/logical matrix aligned to the
ordered Base or an edge list whose endpoints are canonical element IDs.

Relations represent additional empirical pairwise information, including
structural connectivity, functional connectivity, morphological similarity,
gene coexpression, and effective connectivity. Distance, adjacency, and
spatial weights are analysis objects and are not Relation types.

## Layer-view contract

`ngeo_layer_view(x, layer)` selects exactly one layer and returns these stable
fields:

```text
base / values / measure / metadata
```

`values` remains a one-column matrix-like block so element-by-layer alignment
is explicit. The function does not change the normalized storage of the source
dataset. Consumers can therefore depend on the view contract instead of the
internal relationship among `x$values`, `x$layers`, and `x$measures`.

## Base identity

`base_hash()` remains the implementation-specific xxHash64 identity used by R
objects for fast binding. `base_signature()` hashes the canonical portable
manifest-schema-2 Base payload with SHA-256. The signature includes ordered
elements, geometry, coordinate space, intrinsic topology where represented,
and the selected active surface coordinates. Labels are excluded.

## Package boundary

neurogeo owns spatial representation (Base, Layer, optional Relation) and
spatial analysis (distance, neighborhood, weights, spatial statistics, null
models, support mapping, transforms, inference, and uncertainty). Dynamics,
operators for dynamical systems, state, perturbation, simulation, solvers,
calibration, prediction, digital-twin abstractions, receptor-specific
semantics, and neural-mass models are outside this package. A downstream
simulation package may depend on neurogeo; neurogeo must not depend on it.
