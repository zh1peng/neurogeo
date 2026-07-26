# Migrating from neurogeo 3.0 to 3.1

Version 3.1 is additive. No 2.x or 3.0 export is removed, renamed, or
deprecated. Existing eager readers retain their behavior.

Use a `*_filebacked()` reader when an operation can consume delayed chunks
and the complete values matrix should not be retained in memory. The returned
object keeps the same domain, maps, measures, space, and validation contract
as its eager counterpart; only the values storage implementation changes.

Resource limits apply to every block requested from file-backed values, not
to the small metadata object itself. Choose chunk sizes that fit the declared
budget. The default checksum verification is strongest; `verify = "metadata"`
is faster for repeated large reads but detects only size or modification-time
changes, and `verify = "none"` is an explicit loss of mutation protection.

`write_ngeo_filebacked()` is a byte-preserving atomic copy for a complete
selection. It deliberately rejects partial objects. To encode a selected
subset as a new file, materialize that intentional subset within budget and
use the corresponding NIfTI, CIFTI, or FreeSurfer writer.

Object manifests now recognize `ngcs/file-values` schema 3.1 and bind its
source identity. The language-independent conformance corpus defaults to
3.1; callers requiring a historical fixture set should request
`ngeo_conformance_manifest(version = "3.0")`.
