# Source identity and path privacy in 6.0

Status: accepted for the 6.0 audit candidate.

- New reader history uses a logical `file:<basename>#sha256:<prefix>` ID and a
  complete `checksum_sha256`; it does not store an absolute input path.
- `checksum_md5` remains readable and is still emitted during 6.x only for
  compatibility with existing history consumers. It is not the canonical
  identity and is scheduled for removal in the next major version.
- File-backed objects retain an absolute path only in their runtime source
  handle. Shared history and logical identity hashes use `source_id`, SHA-256,
  byte size, format, selection, and binary contract, so identical bytes copied
  to a different machine have the same identity.
- History export redaction continues to remove legacy absolute source IDs and
  checksum fields. A replay manifest must refer to an input artifact by logical
  ID and verify the SHA-256 after resolving it locally.
