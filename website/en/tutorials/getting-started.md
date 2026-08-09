---
title: "15-minute quickstart: from a fixed fixture to interpretable Moran's I"
outline: [2, 3]
editLink: false
sourceSha256: "e88ab1cf8a5bb3f29ac64a40833888b0e51879a278c6546a3a7e21cad0ce2f2b"
---

**Language:** [简体中文](/tutorials/getting-started)
**Edit source:** [Edit on GitHub](https://github.com/zh1peng/neurogeo/edit/main/vignettes/getting-started.Rmd)


## Who is this for?

Start here if you know MRI, fMRI, cortical surfaces, or ROI tables but
are new to `neurogeo`. The tutorial takes about 10–15 minutes and less
than 50 MB of memory. It runs in a clean R session with data installed
in the package—there are no placeholder paths or downloads.

By the end, you will be able to:

1.  distinguish a `base`, `values`, a `layer`, and a `measure`;
2.  construct and validate a spatial dataset;
3.  declare neighbors and spatial weights explicitly;
4.  run and interpret global Moran’s I;
5.  inspect the null, metric, and sampling unit of the result.

## Four terms first

A neuroimaging array alone does not say where each row lives or whether
values may be summed. `neurogeo` keeps four kinds of information
separate:

| Term | Question | This example |
|----|----|----|
| spatial base | Where does each value row live? | 9 two-dimensional points |
| values | What was measured? | one `signal` column |
| layer | Which values column is analysed? | `signal` |
| measure | What are its unit and support semantics? | a.u. and intensive |

`intensive` means aggregation should use a support-weighted mean. It
differs from `extensive` or `count` values that may be summed.
Coordinate space is also explicit: the default `unknown` does not mean
millimetres.

## 1. Read a fixed installed fixture

The CSV is project-generated CC0 teaching data. Its licence, version,
size, and SHA-256 are recorded in the installed
`tutorial-fixtures-6.0.csv`.

``` r
fixture_path <- system.file(
  "extdata", "golden", "tiny-point-grid.csv",
  package = "neurogeo",
  mustWork = TRUE
)
fixture <- read.csv(fixture_path)
fixture
#>   element_id x_mm y_mm signal
#> 1   point-01    0    0      1
#> 2   point-02    1    0      2
#> 3   point-03    2    0      3
#> 4   point-04    0    1      2
#> 5   point-05    1    1      4
#> 6   point-06    2    1      7
#> 7   point-07    0    2      3
#> 8   point-08    1    2      7
#> 9   point-09    2    2      9
```

Do not infer row order from similar coordinates. `element_id` is the
stable source identifier here; row *i* of `values` must always belong to
element *i* of the base.

## 2. Construct the object and declare measurement semantics

``` r
point_data <- ngeo_point(
  coordinates = as.matrix(fixture[c("x_mm", "y_mm")]),
  values = cbind(signal = fixture$signal),
  measures = ngeo_measure(
    measure_id = "measure_signal",
    name = "teaching signal",
    value_type = "continuous",
    support_behavior = "intensive",
    unit = "a.u."
  ),
  coordinate_space = ngeo_coordinate_space(
    space_id = "synthetic-grid",
    kind = "unknown",
    unit = "mm"
  )
)
point_data
#> <ngeo_point>
#>   base: point
#>   elements: 9
#>   layers: 1
#>   coordinate_space: synthetic-grid
```

Inspect it through public accessors. A normal workflow should not mutate
`$measures` or another internal table.

``` r
ngeo_base_type(point_data)
#> [1] "point"
head(ngeo_base_elements(point_data))
#>         element_id source_index source_index_base included
#> 1 element_00000001            1                 1     TRUE
#> 2 element_00000002            2                 1     TRUE
#> 3 element_00000003            3                 1     TRUE
#> 4 element_00000004            4                 1     TRUE
#> 5 element_00000005            5                 1     TRUE
#> 6 element_00000006            6                 1     TRUE
ngeo_layers(point_data)
#>     layer_id   name     measure_id
#> 1 layer_0001 signal measure_signal
ngeo_measures(point_data)
#>       measure_id            name unit value_type support_behavior
#> 1 measure_signal teaching signal a.u. continuous        intensive
#>   missing_policy           aggregation
#> 1       preserve support_weighted_mean
ngeo_validate(point_data, "strict")
```

You should have 9 spatial elements and one layer referencing
`measure_signal`.

## 3. Declare the spatial relation

Points have no intrinsic adjacency. Here Euclidean distance connects
horizontal or vertical neighbors no more than 1.01 mm apart. This is an
analysis choice, not a hidden property of the input.

``` r
spatial_weights <- ngeo_spatial_weights(
  point_data,
  method = "distance_band",
  threshold = 1.01,
  distance_method = "euclidean",
  style = "W"
)
spatial_weights
#> <ngeo_spatial_weights>
#>   method: distance_band
#>   elements: 9
#>   nonzero: 24
#>   normalization: W
#>   components: 1
```

KNN, another threshold, or surface geodesic distance would define a
different estimand and must be reported.

## 4. Run global Moran’s I

``` r
global_result <- ngeo_moran(
  point_data,
  spatial_weights,
  layer = "signal",
  permutations = 199,
  seed = 2026
)
global_result
#> <ngeo_global_stat>
#>   statistic: Moran's I
#>   estimate: 0.532491
#>   expectation: -0.125
#>   observations: 9
#>   permutations: 199
#>   p-value: 0.015
```

A positive Moran’s I means neighboring signal values tend to be similar.
The permutation p-value asks how often an equally extreme statistic
appears when the spatial weights stay fixed and value labels are
exchangeable under the stated rule. It is not the probability that a
biological mechanism exists.

Check the common interpretation contract:

``` r
contract <- ngeo_inference_contract(global_result)
contract
#> <ngeo_inference_contract> ngeo_global_stat
#>   estimand: global spatial autocorrelation statistic
#>   sampling unit: base elements
#>   null model: value-label permutation when requested
#>   metric: distance_band
#>   support: base elements
#>   uncertainty target: permutation distribution of the global statistic
```

Before reporting, verify its estimand, sampling unit, null model,
metric, support, and uncertainty target. If they do not match the study
design, change the analysis—not only the prose.

## 5. Inspect local patterns and history

``` r
local_result <- ngeo_local_moran(
  point_data,
  spatial_weights,
  layer = "signal",
  permutations = 199,
  seed = 2026,
  adjust = "BH",
  null_model = "conditional"
)
local_result[c("element_id", "local_i", "p.adjusted", "cluster")]
#> <ngeo_lisa>
#>   observations: 9
#>   layer: 
#>   permutations:
```

`cluster` is a Moran-quadrant description; call a local pattern
significant only with the adjusted permutation evidence. The object
history records construction:

``` r
ngeo_history(point_data)$operations
#> [[1]]
#> [[1]]$operation
#> [1] "ngeo_point"
#> 
#> [[1]]$software
#> [[1]]$software$package
#> [1] "neurogeo"
#> 
#> [[1]]$software$version
#> [1] "6.0.0"
#> 
#> 
#> [[1]]$timestamp_utc
#> [1] "2026-08-09T10:33:46Z"
#> 
#> [[1]]$parameters
#> [[1]]$parameters$source_index_base
#> [1] 1
```

## Common mistakes

- **Treating the default space as millimetres:**
  `ngeo_coordinate_space()` defaults to `unknown`. Confirm units from a
  reliable header or user declaration before physical-distance analysis.
- **Reordering values by similar names:** the package does not guess
  alignment. Preserve and check stable element IDs.
- **Mutating the measure table:** use `ngeo_measure()` at construction
  and `ngeo_update_measure()` later.
- **Treating a layer name as a permanent ID:** display names may repeat;
  store `layer_id` in reproducible scripts.
- **Using an experimental null as stable inference:** surface spin and
  the Moran eigen-sign surrogate remain uncalibrated opt-in methods and
  do not replace this stable permutation path.

## Reusable reporting sentence

> We analysed an intensive signal on nine ordered spatial elements using
> row-standardized spatial weights from a 1.01 mm Euclidean distance
> band. Uncertainty for global Moran’s I used 199 value-label
> permutations with a fixed seed. Interpretation is limited to the
> declared base, metric, support, and null.

Next, choose the [NIfTI, surface, CIFTI, or ROI/cohort
workflow](/en/tutorials/format-workflows), or continue to [neighbors,
distances, and spatial weights](/en/tutorials/neighbors-and-weights).
