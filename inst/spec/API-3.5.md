# neurogeo 3.5 API

- Scientific identity: `ngeo_logical_hash()`.
- Provenance: `ngeo_provenance_dag()` and
  `ngeo_validate_provenance_dag()`.
- Replay: `ngeo_environment_snapshot()`, `ngeo_replay_step()`,
  `ngeo_record_replay()`, `ngeo_validate_replay_manifest()`,
  `ngeo_replay()`, and replay-manifest JSON I/O.
- Artifacts: artifact-manifest construction, validation, JSON I/O, and
  atomic derivative-batch write/read/validation.

The replay operation whitelist is intentionally closed. The API does not
evaluate manifest-supplied code and does not orchestrate complete BIDS
datasets.
