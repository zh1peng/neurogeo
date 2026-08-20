# neurogeo <img src="man/figures/logo.png" align="right" height="180" alt="neurogeo logo" />

`neurogeo` is the R reference implementation of the Neuroimaging
Geoinformatics Core Specification (NGCS).

> **Development status (6.3):** the NGCS 6.0 data model remains frozen.
> The additive 6.2 brain-GIS inference API is undergoing correctness,
> documentation, and release-evidence review.
> Surface-spin remains experimental. Centered singleton Moran spectral
> randomization now preserves its stated algebraic invariants; scientific use
> still requires a null appropriate to the study design.

The 6.0 data model has one sentence:

> A spatial base defines where data live; layers contain the data observed on
> that base; measures describe what those values mean.

An `ngeo` object therefore contains five top-level components:

```text
ngeo
├── base
├── values
├── layers
├── measures
└── history
```

`values[i, j]` is always aligned with `spatial_base(x)$elements[i, ]` and
`layers(x)[j, ]`. Each layer refers to one row in `measures(x)` through
`measure_id`.

## Installation

There is not yet a signed or tagged 6.2 release. Do not treat the moving
`main` branch as a stable release. From a local source checkout, install the
audited source currently in that checkout with:

```sh
R CMD build .
R CMD INSTALL neurogeo_6.3.0.tar.gz
```

On Windows PowerShell, use `R.exe CMD ...` if `R` is an existing shell alias.
The mutable development build is available only for evaluation:

```r
install.packages("remotes")
remotes::install_github("zh1peng/neurogeo@main")
```

See the [platform commands and optional-backend
matrix](https://zh1peng.github.io/neurogeo/en/guide/installation) before
reading neuroimaging files. A stable command pinned to `v6.3.0` will be
published only after that tag and its release evidence exist.

## Minimal workflow

```r
library(neurogeo)

coordinates <- as.matrix(expand.grid(x = 0:2, y = 0:2))
x <- ngeo_point(
  coordinates,
  values = cbind(signal = c(1, 2, 3, 2, 4, 7, 3, 7, 9)),
  measures = ngeo_measure(
    support_behavior = "intensive",
    unit = "a.u."
  ),
  coordinate_space = ngeo_coordinate_space(
    space_id = "example-grid",
    kind = "unknown",
    unit = "mm"
  )
)

ngeo_spatial_base(x)
ngeo_values(x)
ngeo_layers(x)
ngeo_measures(x)
ngeo_history(x)

w <- ngeo_spatial_weights(
  x,
  method = "distance_band",
  threshold = 1.01,
  distance_method = "euclidean",
  style = "W"
)
ngeo_moran(x, w, layer = "signal", permutations = 999, seed = 2026)
```

The five spatial-base types are `point`, `surface`, `volume`, `parcellation`,
and `grayordinate`. Type-specific coordinates, meshes, voxel grids, affine
matrices, memberships, and brain-model mappings live under `base$geometry`.
`coordinate_space` is part of the base; topology is optional. Distance methods,
spatial weights, transforms, and support mappings are analysis objects rather
than mandatory dataset fields.

## Multiple layers and measures

One hundred subjects with DK68 cortical thickness are represented by a
68-by-100 values matrix, 100 layer rows, and one measure row:

```r
x <- ngeo_parcellation(
  parcels,
  values = thickness,
  layers = data.frame(
    layer_id = sprintf("subject_%03d", 1:100),
    measure_id = "cortical_thickness",
    subject = sprintf("sub-%03d", 1:100)
  ),
  measures = data.frame(
    measure_id = "cortical_thickness",
    name = "cortical thickness",
    unit = "mm",
    value_type = "continuous",
    support_behavior = "intensive",
    aggregation = "area_weighted_mean"
  )
)
```

Layer and measure metadata may be omitted for simple inputs; constructors
generate valid unknown metadata and operations validate capabilities when they
need them.

## Stable layer and relation interfaces

Downstream packages can request one complete spatial field without depending
on the normalized `x$values`, `x$layers`, and `x$measures` storage:

```r
field <- ngeo_layer_view(x, "subject_001")
field$base
field$values
field$measure
field$metadata
```

Optional empirical pairwise information is represented independently rather
than added to the frozen `ngeo` top-level fields:

```r
edges <- data.frame(from = c(1, 2), to = c(2, 3), value = c(.2, .5))
relation <- ngeo_relation(
  x,
  edges,
  type = "functional_connectivity",
  directed = FALSE,
  weighted = TRUE,
  provenance = list(source = "example")
)
ngeo_validate_relation(relation, x)
```

`base_hash()` is the R implementation identity used for fast in-package
binding. `base_signature()` is the canonical SHA-256 identity intended for
cross-language alignment. Labels are excluded from both identities.

Relations are reserved for empirical pairwise information such as structural
or functional connectivity, morphological similarity, gene coexpression, and
effective connectivity. Distance, adjacency, and spatial weights remain
analysis objects.

## Package boundary

neurogeo owns spatial representation (`Base`, `Layer`, and optional
`Relation`) plus spatial analysis, including distance, neighborhood, weights,
statistics, null models, support mapping, transforms, inference, and
uncertainty. Dynamics, perturbation, simulation, calibration, prediction, and
domain-specific neural models belong in downstream packages that consume
neurogeo objects. The dependency direction is downstream package to neurogeo;
neurogeo has no dependency on a simulation or virtual-brain package.

## Spatial aggregation

User-facing aggregation is `aggregate_to()`. It performs a formal change of
spatial support using a declared support map and the measure's aggregation
semantics:

```r
atlas <- ngeo_atlas_map(surface, atlas_labels)
regional <- aggregate_to(surface, atlas$target, atlas)
```

No reader or analysis silently registers, resamples, segments, or converts an
incompatible spatial base.

## Supported inputs

- NIfTI through `RNifti`;
- GIFTI geometry, metric arrays, and labels through `gifti`;
- CIFTI dscalar, dlabel, and dtseries through `cifti`;
- FreeSurfer surface, annot, curv, MGH, and MGZ through
  `freesurferformats`;
- ordinary coordinates, faces, arrays, labels, and affine matrices through
  native constructors.

FreeSurfer, FSL, and Connectome Workbench are not runtime dependencies.

## Validation

```r
ngeo_validate(x, "basic")
ngeo_validate(x, "strict")
ngeo_capabilities(x)
```

Construction enforces alignment. Operations add capability requirements only
when needed: plotting requires geometry, geodesic analysis requires surface
geometry and topology, spatial statistics require spatial weights, aggregation
requires support and measure semantics, and cross-space mapping requires a
coordinate space and transform.

See the [Chinese guide](https://zh1peng.github.io/neurogeo/guide/),
[tutorial index](https://zh1peng.github.io/neurogeo/tutorials/), and
[API reference](https://zh1peng.github.io/neurogeo/api/reference/).
