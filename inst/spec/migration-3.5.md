# Migrating from neurogeo 3.4 to 3.5

Existing objects remain valid. `ngeo_migrate_schema(x, "3.5")` adds an
explicit migration audit attribute.

Declare reproducible operations with named inputs and `ngeo_replay_step()`,
then record and verify them with `ngeo_record_replay()` and `ngeo_replay()`.
Arbitrary manifest code is unsupported.

Publish related derivatives through `ngeo_write_artifact_batch()` and
verify them with `ngeo_read_artifact_batch()`. Batch scope is fixed to
`derivative_only`; complete BIDS orchestration remains out of scope.
