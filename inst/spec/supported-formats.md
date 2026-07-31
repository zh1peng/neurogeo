# Supported standard formats

Status: reviewed for neurogeo 4.4.2

## Input

| Input | Backend | Output | External binary |
|---|---|---|---|
| NIfTI-1/2 `.nii`, `.nii.gz` | RNifti | volume | none |
| GIFTI surface | gifti | surface | none |
| GIFTI metric/shape/functional | gifti | aligned surface maps | none |
| GIFTI label | gifti | labels | none |
| CIFTI dscalar | cifti | grayordinates | none |
| CIFTI dlabel | cifti | grayordinates + label tables | none |
| CIFTI dtseries | cifti | grayordinates + time axis | none |
| FreeSurfer surface | freesurferformats | surface | none |
| FreeSurfer curv/morphometry | freesurferformats | aligned surface maps | none |
| FreeSurfer annot | freesurferformats | labels | none |
| MGH/MGZ volume | freesurferformats | volume | none |
| MGH/MGZ morphometry | freesurferformats | aligned surface maps | none |

## Output and exchange

| Output | Backend | Round-trip scope | External binary |
|---|---|---|---|
| NIfTI `.nii`, `.nii.gz` + explicit mask | RNifti | voxel mapping, affine, map order, values, mask | none |
| GIFTI surface/metric/label files | freesurferformats R writer | geometry, coordinate sets, map order, values, labels | none |
| CIFTI `.dscalar.nii` | neurogeo pure-R CIFTI-2 writer | brain models, map order, values, NamedMap metadata, float32/float64 | none |
| CIFTI `.dlabel.nii` | neurogeo pure-R CIFTI-2 writer | brain models, label axis/tables, int32 values | none |
| CIFTI `.dtseries.nii` | neurogeo pure-R CIFTI-2 writer | brain models, regular time axis, float32/float64 values | none |
| FreeSurfer surface/curv/annot | freesurferformats | geometry, map order, values, labels | none |
| FreeSurfer MGH/MGZ | freesurferformats | affine, map order, values | none |
| BIDS derivative data + JSON | format-specific writer + jsonlite | explicit entities, space, semantics, provenance, pair checksums | none |
| NGCS support map schema 1 | Matrix + jsonlite | monolithic Matrix Market operator and JSON metadata | none |
| NGCS support map schema 2 | Matrix + jsonlite | checksummed chunk bundle and complete logical hash | none |

The CIFTI reader uses the pure-R `cifti` package. The CIFTI writer is
implemented directly in neurogeo and does not invoke Connectome Workbench.
FreeSurfer, FSL, and Connectome Workbench are not runtime dependencies.

## Controlled 4.1 reference data

`ngeo_example_data()` lists six small upstream format fixtures bundled for
interoperability testing and tutorials. The manifest records immutable
source commits and URLs, source package versions, licenses, byte sizes,
SHA-256 values, roles, and expected positive or negative use. Verification
is enabled by default.

The suite covers a NIfTI volume, GIFTI surface geometry, CIFTI fsLR dscalar,
FreeSurfer surface and curv failure cases, and an MGH volume that requires an
explicit affine. These are format fixtures rather than clinical data.

## Download-only 4.2.2 validation data

The 4.2.2 manifest names real Conte69 dscalar/dlabel/dtseries files, HCP 32k
bilateral GIFTI surfaces, and a 256-cubed FreeSurfer MGZ. They are not
installed or redistributed by neurogeo. Release tooling downloads immutable
upstream bytes to an ignored cache and verifies size and SHA-256 before use.
The manifest records original terms separately from upstream R package
licenses.

The 4.3 cartography validation reuses the checksum-verified HCP 32k GIFTI
surface from this download-only cache.

The 4.3.1 manifest adds checksum-pinned HCP S1200 Conte69 flat and
midthickness surfaces, atlas-ROI masks, and Schaefer 2018 Conte69 GIFTI label
files. The real-flatmap gate verifies a registered face-subset chart and
renders bilateral continuous and atlas maps. No surface or atlas is embedded
as a fixed plotting template.

## Safety rules

- NIfTI qform and sform are both retained; conflict emits a structured
  warning and the selected active affine is recorded.
- A zero-valued voxel is never treated as background unless the caller asks
  for `mask = "nonzero"`.
- GIFTI and FreeSurfer metric/label arrays must match the geometry vertex
  count.
- Multiple surface coordinate files must have identical faces.
- Ambiguous MGH/MGZ input requires an explicit domain.
- CIFTI brain-model offsets and counts must form a contiguous ordered
  mapping.
- CIFTI surface geometry is optional. Missing geometry disables surface
  topology, area, and geodesic capabilities.
- Attached CIFTI surfaces must match the declared full vertex count.
- CIFTI map axes, label tables, time axes, and datatypes must satisfy the
  explicit 2.9 contract before writing.
- Support-map schema-2 chunks are not read until sizes and SHA-256 checksums
  have been verified.

## BIDS boundary

Readers preserve adjacent NIfTI JSON metadata and parse relevant spatial
filename entities into source provenance. Writers build and validate
explicitly named derivatives and atomically promote each data-sidecar pair.
They do not index datasets, execute pipelines, or orchestrate a complete
BIDS dataset.
