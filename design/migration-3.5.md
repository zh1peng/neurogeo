# Migrating from neurogeo 3.4 to 3.5

Existing NGCS objects and modelling APIs are unchanged. Use
`ngeo_migrate_schema(x, "3.5")` only when an explicit migration audit
attribute is required.

For reproducible analyses, replace ad hoc serialized closures with named
inputs and `ngeo_replay_step()` declarations. Record the executed workflow
with `ngeo_record_replay()` and exchange it through the replay-manifest JSON
functions. Only documented deterministic neurogeo operations are accepted.

For derivative delivery, replace independently written files and informal
checksums with `ngeo_write_artifact_batch()`. The target directory is
published only after all files and both manifests are complete. Verify a
received batch with `ngeo_read_artifact_batch()` before using any artifact.

The batch scope is always `derivative_only`. Full BIDS discovery,
preprocessing, registration, and dataset orchestration remain outside
neurogeo.
