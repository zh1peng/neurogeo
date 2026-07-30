# neurogeo <img src="man/figures/logo.png" align="right" height="180" alt="neurogeo logo" />

`neurogeo` is the R reference implementation of the Neuroimaging
Geoinformatics Core Specification (NGCS).

The project starts from one deliberately small contract:

```text
one spatial domain
+ one aligned values block
+ explicit space, topology, metric, measurement semantics, and provenance
```

## Installation

Install the development version from the public GitHub repository:

```r
install.packages("remotes")
remotes::install_github(
  "zh1peng/neurogeo"
)
```

To install a local release archive:

```r
install.packages(
  "neurogeo_4.3.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

## Change of support

Spatial aggregation uses an explicit, aligned support map:

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

## Cortical cartography

Version 4.3 draws the source surface itself rather than selecting a fixed
atlas template. Add an imported flat chart, parameterize an already-cut disk,
or create an explicitly non-metric view; then bind any aligned vertex values
and atlas:

```r
flat <- ngeo_flatten_surface(
  surface_disk,
  method = "harmonic",
  boundary = ordered_boundary
)
cortex <- ngeo_cortical_map(
  flat,
  values = vertex_values,
  atlas = atlas_labels
)
plot(cortex)
plot_data <- ngeo_cortical_map_data(cortex)
```

Harmonic parameterization rejects closed surfaces and never invents a cut.
Orthographic, PCA, and spherical results from `ngeo_project_surface()` are
labelled viewing projections, not registration or metric flattening. See the
[4.3 cartography contract](design/cortical-cartography-4.3.md).

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

## Scientific validation

Version 4.2 independently compares matched spatial statistics and model
quantities with `spdep`, `spatialreg`, `gstat`, and `GWmodel`, then applies
seeded calibration gates. The exact estimands, tolerances, simulations, and
non-claims are recorded in
[the 4.2 scientific-validation contract](design/scientific-validation-4.2.md).

Start with the
[Chinese learning path](https://zh1peng.github.io/neurogeo/guide/), then
complete the visual walkthrough from a spatial object to
[Moran's I](https://zh1peng.github.io/neurogeo/tutorials/getting-started).
The [tutorial index](https://zh1peng.github.io/neurogeo/tutorials/) and
[API reference](https://zh1peng.github.io/neurogeo/api/reference/) are built
from the same R package sources.

The supported entry points are grouped in the
[4.2.1 API tiers](design/API-tiers-4.2.1.md), with compatibility notes in the
[4.3 migration record](design/migration-4.3.md). The
[4.2.2 real-data contract](design/real-data-validation-4.2.2.md) records
download-only fixture governance, exercised scales, and non-claims. Use
`citation("neurogeo")` for the installed citation.
