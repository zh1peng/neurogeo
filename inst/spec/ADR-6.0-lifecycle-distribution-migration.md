# ADR: 6.x lifecycle, distribution, and 5.x migration

Status: accepted for implementation, 2026-08-09.

## Lifecycle

- Every namespace export, registered S3 method, public generic, and public
  `ngeo_*` class is listed in `api-lifecycle-6.0.csv` with one lifecycle state,
  owner, replacement, and earliest removal version.
- The accepted Phase 1 baseline contains 238 exports and 98 registered S3
  methods, including the migration report and inference-contract methods. Any later count change
  requires an ADR update together with regenerated lifecycle and contract
  registries.
- `ADR-6.1-brain-gis-promotion.md` is the accepted additive amendment: the 6.1
  baseline contains 248 exports and 99 registered S3 methods. The NGCS 6.0
  container and the remaining 6.x compatibility rules are unchanged.
- The unprefixed accessors (`values()`, `layers()`, `measures()`, `history()`,
  `spatial_base()`, `base_elements()`, `base_type()`, and `base_hash()`) remain
  functional without warning for all 6.x releases. Their preferred replacements
  are the corresponding `ngeo_*` names.
- `ngeo_validate_layers()` remains functional in 6.x; new code should use
  `ngeo_layer_index()` and feature terminology.
- Removal is permitted only in the next major release after at least two minor
  releases carrying documentation and lifecycle metadata. In that major
  release, attaching neurogeo must no longer mask `utils::history()`.
- Surface spin, the Moran eigen-sign surrogate, spatial ordination,
  coregionalization, and MGWR are experimental. Availability is not evidence of
  inferential calibration.

## Distribution

- Development builds are identified by an exact source commit and tar SHA-256.
- A stable installation instruction must point to an immutable tag/release, not
  mutable `main`.
- The first public 6.x prerelease requires the release evidence registry,
  cross-platform checks, signed tag, source/binary hashes, citation metadata,
  and archived DOI workflow described by REL-201.

## 5.x migration support matrix

| Legacy object | Automated 6.x migration | Required evidence |
|---|---|---|
| point | supported | coordinates, ordered values, layer IDs, measure semantics |
| surface | supported when coordinates/faces are present | coordinate roles, faces, mask, space |
| volume | supported when dimensions/affine/source voxel index are present | affine and voxel-order golden object |
| parcellation/regions | supported when region table and support are present | region IDs, membership/centroid/support |
| grayordinate | supported when ordered component definitions are complete | structure, vertex/voxel indices, affine |
| delayed/file-backed or unknown extension | reconstruction report only | source URI/checksum and unsupported fields |

Migration never infers registration, coordinate roles, physical units, support
semantics, or missing geometry. Unsupported input returns a structured
reconstruction report rather than a partially valid object.
