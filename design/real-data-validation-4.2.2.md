# Real-data validation contract for neurogeo 4.2.2

## Evidence boundary

The 4.2.2 report validates format parsing/writing, ordered element alignment,
explicit spaces and measurement semantics, sparse/file-backed resource
behavior, support conservation, uncertainty propagation, and auditable
provenance. It does not validate preprocessing, anatomical registration,
clinical interpretation, population inference, or universal performance.

## Fixture governance

External files remain outside the source archive. The manifest records the
immutable source commit and URL, byte size, SHA-256, upstream package
license, original data terms, and `download-only` policy as separate fields.
No package license is treated as a substitute for data-use terms.

## Required workflows

1. NIfTI: affine and active-mask preservation, aligned values, sparse voxel
   weights, Moran execution, write/read identity, and sidecar domain hash.
2. Surface and FreeSurfer: both HCP 32k hemispheres, triangle topology,
   vertex-aligned metric, sparse contiguity, GIFTI and generated FreeSurfer
   round-trips, and bounded real MGZ file-backed reads.
3. CIFTI: real dscalar, dlabel, and dtseries files; ordered brain models,
   label and time axes, partial file-backed reads, and pure-R round-trips.
4. Atlas/change of support: a real Conte69 dense-label domain, crisp and
   sparse probabilistic membership, partial coverage, non-uniform source
   support, intensive/extensive semantics, uncertainty, and a checksummed
   support-bundle derivative.

## Adversarial and resource gates

The report must reject source mutation, malformed label tables, invalid
CIFTI datatypes, missing surface vertices, and truncated CIFTI/GIFTI files.
It must exercise partial atlas coverage and disconnected topology without
silently changing the declared semantics.

Surface and atlas operators remain sparse. CIFTI dtseries and MGZ reads are
file-backed with an explicit 10,000-element materialization ceiling. The
report records 32k surfaces, 60,951 grayordinates, and a bounded 256-cubed
MGZ selection. It must identify 164k surface and 91k grayordinate cases as
not exercised when compatible fixtures are unavailable.

All workflows use R backends. FreeSurfer, FSL, and Connectome Workbench are
not runtime, validation, or CI dependencies.
