# Migration to neurogeo 3.6 and 4.0

## Change of support

Replace:

```r
ngeo_change_support_block(x, target, block_map)
```

with:

```r
ngeo_change_support(x, target, block_map)
```

In 4.0, use an ordinary sparse `ngeo_support_map`; target-row batching is an
execution detail rather than a second scientific object.

## Validation

Replace object-specific validation calls with:

```r
ngeo_validate(object)
```

Format-contract validators used at external I/O boundaries remain separate.

## Metrics

Pass controlled metric names such as `"euclidean"`, `"edge_geodesic"`, or
`"hops"` directly. Parameterized metric objects never had execution semantics
and are removed in 4.0.

## Execution and replay

Generic task plans and content caches are not scientific NGCS objects. Use
`ngeo_record_replay()` and `ngeo_replay()` for auditable supported workflows.
Package-internal atomic writes and caches are not part of the 4.0 public API.

## Models

Replace `ngeo_gwr_batched()` with `ngeo_gwr()` and
`ngeo_kriging_batched()` with `ngeo_kriging()`. Both base implementations
already evaluate targets incrementally.
