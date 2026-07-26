# Migration to neurogeo 4.0

## Metrics

Pass a controlled metric name directly:

```r
ngeo_distance(x, metric = "edge_geodesic")
```

Parameterized metric objects were rejected because their parameters had no
execution semantics.

## Change of support

Use one ordinary sparse support map:

```r
ngeo_change_support(
  x,
  target,
  support_map,
  budget = ngeo_resource_budget()
)
```

Remove calls to block-map constructors, materializers, diagnostics,
composition, variance, and `ngeo_change_support_block()`. Sparse matrix
execution and target-row chunking are implementation details.

## Validation

Replace registry lookup and object-specific reporting with:

```r
ngeo_validate(object)
```

Use `ngeo_object_manifest()` when a portable, canonical metadata record is
needed. Schema migration in 3.x only attached an attribute and is removed;
4.0 does not silently rewrite objects.

## Bounded values and output

Use format-specific file-backed readers and `ngeo_value_chunks()` instead of
constructing delayed values. Use public writers, replay-manifest writers, or
artifact-batch writers instead of the internal atomic-write helper.

## Models and replay

Use `ngeo_gwr()` and `ngeo_kriging()` directly; both already evaluate
targets incrementally. Use `ngeo_record_replay()` and `ngeo_replay()` for
auditable supported workflows. Generic task plans and caches are not NGCS
scientific objects.

## Removed introspection

The R namespace and these migration notes are the public API contract.
Conformance fixtures and schema definitions remain installed verification
resources, not runtime registries.
