# neurogeo 2.6 API

- `ngeo_resource_budget()` declares bounded work.
- `ngeo_execution_plan()` and `ngeo_execute()` provide deterministic,
  identity-bound, resumable task execution.
- `ngeo_cache()` and `ngeo_cache_compute()` provide content-addressed cached
  results; `ngeo_atomic_write()` safely publishes final output.
- `ngeo_change_support_block()`, `ngeo_block_diagnostics()`,
  `ngeo_block_variance()`, and `ngeo_compose_block_support_map()` operate on
  sparse support blocks.
- `ngeo_stream_summary()`, `ngeo_stream_covariance()`, `ngeo_stream_lm()`,
  and `ngeo_stream_moran()` consume aligned delayed chunks.

All neurogeo 2.5 APIs remain available.
