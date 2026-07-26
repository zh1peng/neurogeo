# Migrating from neurogeo 2.9.1 to 3.0

Version 3.0 is additive for existing R callers. No 2.x export is removed,
renamed, or deprecated.

Existing object-specific validation remains valid. Use
`ngeo_validate_schema()` when a generic workflow needs one structured report
for different object families.

Use `ngeo_object_manifest()` for portable metadata exchange and integrity
checking. This manifest intentionally contains no second values block and is
not a replacement for NIfTI, GIFTI, CIFTI, FreeSurfer, or support-map
exchange files.

`ngeo_migrate_schema()` records an explicit 3.0 migration attribute after
validation; it does not resample, reorder, or otherwise change scientific
content.
