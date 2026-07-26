# neurogeo

`neurogeo` is the R reference implementation of the Neuroimaging
Geoinformatics Core Specification (NGCS).

The project starts from one deliberately small contract:

```text
one spatial domain
+ one aligned values block
+ explicit space, topology, metric, measurement semantics, and provenance
```

## Installation

The development repository is currently private. Authorized users can
install it with a GitHub personal access token:

```r
install.packages("remotes")
remotes::install_github(
  "zh1peng/neurogeo",
  auth_token = Sys.getenv("GITHUB_PAT")
)
```

To install a local release archive:

```r
install.packages(
  "neurogeo_4.0.1.tar.gz",
  repos = NULL,
  type = "source"
)
```

The 4.0 release implements the five NGCS domains, strict alignment,
space and measurement metadata, provenance, native and neuroimaging readers,
sparse topology/distance/weights, semantic parcellation, and foundational
spatial statistics. Version 1.1 adds controlled surface charts, bounded
`sf` export, diagnostics for every domain, Getis-Ord statistics, sparse
correlograms, and unified permutation inference.

Version 1.2 also applies only explicitly supplied affine transforms,
round-trips NIfTI/GIFTI/FreeSurfer outputs without external binaries, scales
coordinate neighbor queries through a bounded KD-tree backend, and exports
redactable provenance.

Version 1.3 adds bounded spatial nulls, OLS/SLX adapters, and
explicit-bandwidth kernel regression. These methods expose their domain,
topology, metric, missing-data, isolate, and simulation assumptions rather
than performing implicit preprocessing.

NGCS 2.0 adds a sparse `target × source` support-map contract for crisp,
probabilistic, and overlapping mappings. Change of support is driven by
measurement semantics, preserves extensive totals under valid allocation,
propagates declared uncertainty, supports cross-atlas overlap/transfer, and
can verify global parcellation-invariant estimates.

NGCS 2.1 adds builders for already-known surface registrations, voxel
affines, hard segmentations, and probabilistic atlases. It also adds sparse
coverage/conservation/entropy diagnostics, operator sensitivity and
uncertainty sampling, atlas-robust inference, and Matrix Market plus JSON
exchange. These functions consume declared spatial relationships; they do
not estimate registration.

NGCS 2.2 adds domain-bound covariance, analytic and Monte Carlo uncertainty,
validated registration/segmentation ensembles, and sparse conditioning.
NGCS 2.3 adds common-source spatial or non-spatial null families, max-T
adjustment, cross-atlas meta-analysis, declared multiscale inference, and
boundary-ensemble tests. These summaries quantify the supplied support
family; they do not claim parcellation invariance.

NGCS 2.4 adds bounded variogram fitting, local kriging, GWR, SAR/SEM, CAR,
and cross-support modelling. NGCS 2.5 adds pure-R CIFTI writing, one-block
delayed values, logically hashed sparse blocks, scoped BIDS derivatives, and
a million-element sparse resource gate. NGCS 2.6 adds deterministic resource
budgets, resumable execution plans, content-addressed caches, atomic output,
true blockwise support operations, and delayed-native streaming statistics.
NGCS 2.7 adds domain-bound uncertainty for variograms, kriging, GWR,
SAR/SEM, Gaussian CAR posteriors, and cross-support model ensembles.
NGCS 2.8 adds exact space registries, explicit aliases, directed supplied
transform graphs, deterministic path selection, diagnostics, and auditable
authorized affine application.
NGCS 2.9 completes the 2.x interoperability boundary with richer pure-R
CIFTI metadata and datatype validation, canonical BIDS derivative
transactions, chunked checksummed support-map bundles, a
language-independent conformance corpus, and an explicit 3.0 API audit.
NGCS 3.0 adds a versioned core-object schema registry, structured validation
reports, canonical portable metadata manifests, explicit schema migration,
and an API lifecycle registry while retaining every 2.x export.
NGCS 3.1 adds resource-bounded file-backed NIfTI, CIFTI, MGH, and MGZ values,
exact frame/brain-model/voxel selections, source-mutation identities,
partial-read provenance, and atomic complete-source copying without adding a
second values block.
NGCS 3.2 adds explicitly authorized transform-aware resampling plans that
bind an already-selected non-lossy affine path to sparse
nearest/trilinear/barycentric/overlap support mapping, measurement-aware
conservation, uncertainty policy, bounded execution, and joint provenance.
NGCS 3.3 adds explicit regular/irregular and instant/interval time axes,
map-aligned temporal measurement semantics, sparse temporal weights,
matrix-free separable space-time lag and Moran statistics, bounded temporal
and joint variograms, and support-aware longitudinal change, trend, and
contrast helpers without duplicating spatial geometry through time.
NGCS 3.4 adds deterministic sparse CG/BiCGSTAB controls, exact-small or
seeded error-diagnosed log determinants, iterative SAR/SEM and Gaussian CAR,
and ordered resource-bounded GWR/kriging batches. Convergence,
non-convergence, approximation error, and dense-reference boundaries are
never hidden.
NGCS 3.5 completes the roadmap with scientific logical hashes, immutable
provenance DAGs, environment-bound whitelist-only replay, verified portable
artifact manifests, and atomically published derivative-only batches.
Mutation, environment drift, dependency errors, incomplete output, and
artifact corruption fail before verified results are returned.

neurogeo 4.0 keeps those NGCS 3.5 scientific contracts and removes
implementation frameworks that had become misleading public abstractions.
Metrics are controlled names, change of support uses one sparse support-map
path, registered objects use one validation entry point, and replay—not a
generic task planner—is the auditable execution contract. Delayed storage,
atomic publication, and conformance catalogs remain internal mechanisms.

```r
target <- ngeo_regions(
  data.frame(region_id = c("A", "B")),
  support_size = c(5, 4)
)
support_map <- ngeo_support_map(
  x,
  target,
  operator = c(rep("A", 5), rep("B", 4)),
  source_support = rep(1, 9)
)
regional <- ngeo_change_support(x, target, support_map)
```

Aligned atlas labels can construct both the operator and target:

```r
atlas <- ngeo_atlas_map(x, labels)
ngeo_support_diagnostics(atlas)
regional <- ngeo_change_support(x, atlas$target, atlas)
```

This is not an MRI preprocessing package. It does not perform registration,
resampling, segmentation, surface reconstruction, or implicit conversion
between incompatible spatial supports.

## Supported inputs

- NIfTI through `RNifti`;
- GIFTI geometry, metric, and label through `gifti`;
- CIFTI dscalar, dlabel, and dtseries through the pure-R `cifti` backend;
- FreeSurfer surface, annot, curv, MGH, and MGZ through
  `freesurferformats`;
- ordinary coordinates, faces, labels, values, arrays, and affines through
  native constructors.

FreeSurfer, FSL, and Connectome Workbench are not runtime dependencies.
Readers never register or resample data implicitly.

Large supported files can remain file-backed:

```r
x <- read_ngeo_cifti_filebacked(
  "sub-01_task-rest_bold.dtseries.nii",
  frames = 1:100,
  budget = ngeo_resource_budget(
    memory_bytes = 64 * 1024^2,
    materialized_elements = 8e6
  )
)
ngeo_value_chunks(x, chunk_size = 4096, FUN = colMeans)
```

## Minimal workflow

```r
library(neurogeo)

coordinates <- as.matrix(expand.grid(x = 0:2, y = 0:2))
x <- ngeo_points(
  coordinates,
  values = cbind(signal = c(1, 2, 3, 2, 4, 7, 3, 7, 9)),
  measures = ngeo_measure(spatial_semantics = "intensive")
)
w <- ngeo_weights(
  x,
  method = "distance_band",
  threshold = 1.01,
  style = "W"
)
ngeo_moran(x, w, map = "signal", permutations = 999, seed = 2026)
```

The same analysis contract applies after importing a supported format:

```r
surface <- read_ngeo_gifti(
  geometry = "subject.L.midthickness.surf.gii",
  data = c(thickness = "subject.L.thickness.shape.gii"),
  labels = c(aparc = "subject.L.aparc.label.gii")
)
surface$measures$spatial_semantics <- "intensive"
w <- ngeo_weights(surface, method = "mesh_contiguity", style = "W")
result <- ngeo_moran(
  surface,
  w,
  map = "thickness",
  permutations = 999,
  seed = 2026
)
```

Start with the
[Chinese getting-started tutorial](vignettes/getting-started-zh.Rmd), then
use the package vignettes for core concepts, format-specific readers,
neighbors and weights, and measurement-aware parcellation.
