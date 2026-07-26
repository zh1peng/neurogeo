# Migrating from neurogeo 1.x to 2.0

Existing `ngeo` datasets and `ngeo_partition` workflows remain valid.

For a crisp partition:

```r
support_map <- ngeo_support_map_from_partition(
  source,
  partition,
  target_regions
)
result <- ngeo_change_support(source, target_regions, support_map)
```

`ngeo_aggregate()` remains available. The support-map path is preferred when
operators are probabilistic, overlapping, composed, compared across atlases,
or accompanied by uncertainty.

Important changes:

- support-map orientation is always target by source;
- unknown measurement semantics must be resolved before change of support;
- extensive overlap requires `allocation = "normalize"` or fails;
- partial coverage requires an explicit unmapped/drop policy and cannot claim
  global conservation;
- atlas transfer requires `model = "piecewise_constant"` and returns its
  operator;
- uncertainty remains separate from the one aligned values block;
- new change-of-support provenance uses NGCS specification version 2.0.
