# neurogeo 3.1 API

## File-backed constructors and validation

- `ngeo_file_values()` constructs one verified element-by-map file-backed
  values block.
- `ngeo_validate_file_values()` verifies its schema, source identity, binary
  contract, selections, alignment metadata, and current source state.
- `ngeo_file_values_identity()` returns the canonical SHA-256 identity used
  by provenance, caches, and checkpoints.

## Readers

- `read_ngeo_nifti_filebacked()` reads NIfTI voxels and frames on demand.
- `read_ngeo_cifti_filebacked()` reads CIFTI brain-model rows and maps on
  demand without loading the complete data matrix.
- `read_ngeo_mgh_filebacked()` reads MGH/MGZ voxels and frames on demand.
- `read_ngeo_filebacked()` dispatches by an explicit or detected format.

All readers accept exact selections, a source-mutation verification policy,
and an `ngeo_resource_budget`. They return an ordinary one-domain `ngeo`
object whose sole values block inherits `ngeo_delayed_values`.

## Output

- `write_ngeo_filebacked()` atomically copies a complete selected source
  using a bounded byte buffer and rejects partial selections.

The existing `ngeo_value_chunks()` interface is the deterministic chunk
iterator. Existing execution-plan, cache, and checkpoint APIs consume the
file-backed values identity; no parallel scheduler API is introduced.

All neurogeo 2.x and 3.0 exports remain available and stable.
