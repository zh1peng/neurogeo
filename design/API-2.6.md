# neurogeo 2.6 API

## Bounded execution

- `ngeo_resource_budget()` declares block, task, byte, element, and elapsed
  limits.
- `ngeo_execution_plan()` creates a deterministic identity-bound task plan;
  `ngeo_execute()` executes or safely resumes it.
- `ngeo_cache()` creates a content-addressed cache;
  `ngeo_cache_compute()` returns a verified hit or computes one value.
- `ngeo_atomic_write()` promotes a completed temporary output and returns its
  SHA-256 checksum.

## Block support

- `ngeo_change_support_block()` consumes sparse blocks directly for
  intensive, extensive, count, categorical, and explicitly resolved unknown
  semantics.
- `ngeo_block_diagnostics()` computes coverage and sparsity diagnostics
  without reconstructing the logical operator.
- `ngeo_block_variance()` propagates independent source variance blockwise.
- `ngeo_compose_block_support_map()` composes compatible block maps without
  materializing either input operator.

## Streaming values

- `ngeo_stream_summary()` and `ngeo_stream_covariance()` use numerically
  stable chunk combination.
- `ngeo_stream_lm()` fits OLS from bounded sufficient statistics.
- `ngeo_stream_moran()` reads only the delayed value slices required by each
  sparse weights row block.

All neurogeo 2.5 APIs remain available.
