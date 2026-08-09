---
title: "Choose a workflow: NIfTI, surface, CIFTI, or ROI/cohort"
outline: [2, 3]
editLink: false
sourceSha256: "3d6be9b550f42522c487afe949841b775bc18f18142d70e122abd2ff7a7de68a"
---

**Language:** [简体中文](/tutorials/format-workflows)
**Edit source:** [Edit on GitHub](https://github.com/zh1peng/neurogeo/edit/main/vignettes/format-workflows.Rmd)


## Choose from the spatial element—not the filename alone

Use this page after the [15-minute
quickstart](/en/tutorials/getting-started). A filename tells you its
container format; the analysis base tells you what one row means.

| Your rows represent | Typical input | Base | First checks |
|----|----|----|----|
| voxels | NIfTI | volume | dimensions, affine, mask, physical unit |
| surface vertices | GIFTI or FreeSurfer | surface | vertex order, faces, coordinate role |
| mixed brain models | CIFTI | grayordinate | component order, structure, vertex/voxel index |
| parcels × subjects | CSV/TSV | parcellation plus subject layers | region IDs, subject IDs, measure semantics |

The installed teaching corpus has one CC0 fixture for each route:

``` r
corpus_path <- system.file(
  "extdata", "tutorial-fixtures-6.0.csv",
  package = "neurogeo",
  mustWork = TRUE
)
corpus <- read.csv(corpus_path)
corpus[c("fixture_id", "format", "workflow", "size_bytes", "sha256")]
#>           fixture_id  format     workflow size_bytes
#> 1         point-grid     CSV   quickstart        163
#> 2       nifti-volume NIfTI-1       volume        129
#> 3      gifti-surface   GIFTI      surface        851
#> 4 cifti-grayordinate CIFTI-2 grayordinate       1792
#> 5         roi-cohort     CSV   ROI/cohort        270
#>                                                             sha256
#> 1 9023273408303de14e1330322b7899011d590c936f2e35cbd19a312e2860cdea
#> 2 2abdd2505e4e96cbeab76c4cee9912da4df95e56e57e64117ce0818b31db9167
#> 3 2f8c2d3ae36fca8e0cd87dd5ea007bf8fca309b7abce2820c16e97ccad4e97c6
#> 4 408e647738e420aa0935a5f59d0fd29fb70edd1ad5ba90294612b16007745f36
#> 5 427e9fe03d3592bd16a5bb4f57af37369fb492fd52437587a3918382850b0317
```

## NIfTI / volume

Start with the affine, not a screenshot. Equal array dimensions do not
imply the same coordinate space.

``` r
nifti_path <- system.file(
  "extdata", "golden", "tiny.nii.gz",
  package = "neurogeo", mustWork = TRUE
)
nifti_path
#> [1] "E:/03_tools/neurogeo/.r-lib/neurogeo/extdata/golden/tiny.nii.gz"
```

Continue with the executable [NIfTI volume
workflow](/en/tutorials/workflow-volume). Report the chosen world-space
distance and verified spatial unit.

## GIFTI or FreeSurfer / surface

A surface needs both ordered vertex coordinates and triangular faces.
Anatomical coordinates may define a metric; registration and
visualization coordinates must not silently do so.

``` r
surface_path <- system.file(
  "extdata", "golden", "tetra.surf.gii",
  package = "neurogeo", mustWork = TRUE
)
surface_path
#> [1] "E:/03_tools/neurogeo/.r-lib/neurogeo/extdata/golden/tetra.surf.gii"
```

Continue with the executable [surface
workflow](/en/tutorials/workflow-surface) after verifying faces,
components, coordinate roles, and masks.

## CIFTI / grayordinate

A grayordinate row belongs to an ordered brain model. It may refer to a
cortical vertex or a subcortical voxel; there is no valid implicit
distance across structures.

``` r
cifti_path <- system.file(
  "extdata", "golden", "tiny.dscalar.nii",
  package = "neurogeo", mustWork = TRUE
)
cifti_path
#> [1] "E:/03_tools/neurogeo/.r-lib/neurogeo/extdata/golden/tiny.dscalar.nii"
```

Check every component’s structure and source indices. Surface distance
also requires matching surface geometry; the CIFTI scalar array alone
does not provide it.

Continue with the executable [CIFTI
workflow](/en/tutorials/workflow-cifti).

## ROI × cohort

Keep region support separate from independent subjects. Regions define
the spatial base; subjects normally belong in the layer table or
group-design object rather than being treated as spatial neighbors.

``` r
roi_path <- system.file(
  "extdata", "golden", "tiny-roi-cohort.csv",
  package = "neurogeo", mustWork = TRUE
)
roi_cohort <- read.csv(roi_path)
roi_cohort
#>   subject_id   group age_years roi_A roi_B roi_C roi_D
#> 1     sub-01 control        25  2.10  2.34  2.48  2.62
#> 2     sub-02 control        31  2.18  2.29  2.55  2.58
#> 3     sub-03 control        37  2.06  2.41  2.51  2.67
#> 4     sub-04    case        28  1.92  2.17  2.31  2.44
#> 5     sub-05    case        34  1.88  2.22  2.27  2.39
#> 6     sub-06    case        40  1.83  2.13  2.24  2.35
```

Before group inference, declare the independent unit and exchangeability
schedule. Do not treat several atlases derived from the same subjects as
independent studies.

Continue with the executable [ROI-by-cohort
workflow](/en/tutorials/workflow-roi-cohort).

## What all four routes share

Every workflow should reach the same checkpoints:

1.  read from a real immutable source;
2.  inspect ordered elements and values;
3.  confirm coordinate space and measurement semantics;
4.  run strict validation and QC;
5.  declare metric, support, and null explicitly;
6.  inspect `ngeo_inference_contract()` for a scientific result;
7.  write output with history and source identity.

The format-specific end-to-end tutorials expand these checkpoints
without changing the core object model.
