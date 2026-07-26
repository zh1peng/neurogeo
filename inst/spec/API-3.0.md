# neurogeo 3.0 API

## Schema discovery

- `ngeo_schema_registry()` returns the versioned registry and API lifecycle.
- `ngeo_schema()` resolves one object or class to its schema descriptor.

## Validation

- `ngeo_validate_schema()` returns a deterministic
  `ngeo_validation_report` or raises `ngeo_error_schema_validation`.
- Existing object-specific validators remain authoritative and public.

## Portable manifests

- `ngeo_object_manifest()` creates canonical portable metadata.
- `ngeo_validate_manifest()` verifies structure, SHA-256, and optional object
  identity.
- `write_ngeo_manifest()` atomically writes JSON.
- `read_ngeo_manifest()` parses and verifies JSON before returning it.

## Lifecycle and migration

- `ngeo_api_lifecycle()` inventories every namespace export.
- `ngeo_migrate_schema()` validates and records explicit migration to 3.0.
- `ngeo_api_inventory()` remains available as the 2.9-boundary compatibility
  view.

All neurogeo 2.x exports remain available and stable.
