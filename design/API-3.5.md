# neurogeo 3.5 API

- `ngeo_logical_hash()` computes budgeted scientific identity without
  provenance timestamp sensitivity.
- `ngeo_provenance_dag()` and `ngeo_validate_provenance_dag()` construct and
  verify immutable acyclic dependency graphs.
- `ngeo_environment_snapshot()` records the deterministic replay
  environment.
- `ngeo_replay_step()`, `ngeo_record_replay()`,
  `ngeo_validate_replay_manifest()`, and `ngeo_replay()` declare, record,
  validate, and verify whitelist-only analyses.
- `write_ngeo_replay_manifest()` and `read_ngeo_replay_manifest()` provide
  atomic portable JSON exchange.
- `ngeo_artifact_manifest()`, `ngeo_validate_artifact_manifest()`,
  `write_ngeo_artifact_manifest()`, and `read_ngeo_artifact_manifest()`
  provide root-relative content integrity.
- `ngeo_write_artifact_batch()`, `ngeo_read_artifact_batch()`, and
  `ngeo_validate_artifact_batch()` atomically publish and verify complete
  derivative-only batches.

All earlier exports remain stable. Replay supports only the documented
deterministic operation whitelist and does not accept functions or source
code.
