---
title: "15-minute quickstart: a DK68 SCZ-control cortical study"
outline: [2, 3]
editLink: false
sourceSha256: "749c8b7f92578dda610e576cd7f468e722b14b40429ddb135df3699fd516c88c"
---

**Language:** [简体中文](/tutorials/getting-started)
**Edit source:** [Edit on GitHub](https://github.com/zh1peng/neurogeo/edit/main/vignettes/getting-started.Rmd)


## The question, before the object

We simulate cortical thickness for 100 healthy controls (HC) and 100
people with schizophrenia (SCZ), measured in the 68 cortical
Desikan–Killiany (DK) parcels. Age, sex, and site are subject-level
covariates. The simulation embeds lower SCZ thickness in bilateral
superior temporal, insular, anterior cingulate, and medial orbitofrontal
cortex, with weaker effects in neighboring parcels. It is teaching data,
not clinical evidence.

``` r
with(dk$design, table(group, site))
#>      site
#> group site-a site-b
#>   HC      50     50
#>   SCZ     50     50
summary(dk$design$age)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#>   18.00   29.20   36.65   36.22   43.48   59.10
dim(ngeo_values(dk$cohort))
#> [1]  68 200
```

The independent sampling unit is the subject. DK parcels are spatial
elements; they are not 68 additional subjects.

## See the brain before computing a statistic

The first map is one HC participant. The second is the descriptive
SCZ-minus-HC contrast. Every subsequent number in this tutorial refers
back to these maps.

``` r
ngeo_tutorial_plot_dk(
  dk$cohort, layer = "sub-001",
  title = "Simulated cortical thickness: sub-001 (HC)"
)
```

<div class="figure" style="text-align: center">

<img src="./getting-started_files/figure-gfm/subject-brain-1.png" alt="DK cortical thickness for one simulated healthy control shown on bilateral cortical views"  />
<p class="caption">

One subject is one layer; DK parcels form the shared spatial base.
</p>

</div>

``` r
ngeo_tutorial_plot_dk(
  dk$difference,
  title = "SCZ minus HC cortical thickness (mm)",
  midpoint = 0
)
```

<div class="figure" style="text-align: center">

<img src="./getting-started_files/figure-gfm/contrast-brain-1.png" alt="Simulated SCZ minus HC cortical thickness difference on the DK atlas"  />
<p class="caption">

The case-control contrast is spatially structured and is plotted before
spatial inference.
</p>

</div>

## Data model: base, values, layers, and measure

| Term    | Neuroimaging meaning here | Shape            |
|---------|---------------------------|------------------|
| base    | DK cortical parcels       | 68 rows          |
| values  | cortical thickness        | 68 × 200         |
| layer   | one subject map           | 200 columns      |
| measure | thickness, intensive, mm  | shared semantics |

``` r
ngeo_base_type(dk$cohort)
#> [1] "parcellation"
head(ngeo_base_elements(dk$cohort)[c("region_id", "hemi", "region", "lobe")])
#>                    region_id hemi                            region      lobe
#> 1                lh_bankssts left banks of superior temporal sulcus  temporal
#> 2 lh_caudalanteriorcingulate left         caudal anterior cingulate cingulate
#> 3     lh_caudalmiddlefrontal left             caudal middle frontal   frontal
#> 4                  lh_cuneus left                            cuneus occipital
#> 5              lh_entorhinal left                        entorhinal  temporal
#> 6                lh_fusiform left                          fusiform  temporal
head(ngeo_layers(dk$cohort)[c("layer_id", "subject_id", "group", "age", "site")])
#>   layer_id subject_id group  age   site
#> 1  sub-001    sub-001    HC 20.5 site-a
#> 2  sub-002    sub-002    HC 25.0 site-b
#> 3  sub-003    sub-003    HC 44.3 site-a
#> 4  sub-004    sub-004    HC 25.2 site-b
#> 5  sub-005    sub-005    HC 29.9 site-a
#> 6  sub-006    sub-006    HC 48.0 site-b
ngeo_measures(dk$cohort)
#>           measure_id               name unit value_type support_behavior
#> 1 cortical_thickness cortical thickness   mm continuous        intensive
#>   missing_policy           aggregation
#> 1       preserve support_weighted_mean
ngeo_validate(dk$cohort, "strict")
```

Row *i* always means the same DK parcel across all 200 columns. Subject
metadata belongs to layers; millimetres and intensive support semantics
belong to the measure. This alignment is the core contract that a
numeric matrix alone does not carry.

## Declare a cortical spatial relation

The teaching fixture includes a fixed symmetric DK neighborhood graph
derived from the atlas drawing. `region_contiguity` uses that graph
directly. For a real study, build adjacency or geodesic weights from the
registered cortical surface and record that provenance; do not infer
scientific adjacency from a figure.

``` r
weights <- ngeo_spatial_weights(
  dk$difference,
  method = "region_contiguity",
  style = "W"
)
weights
#> <ngeo_spatial_weights>
#>   method: region_contiguity
#>   elements: 68
#>   nonzero: 424
#>   normalization: W
#>   components: 2
summary(Matrix::rowSums(weights$raw_matrix != 0))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#>   3.000   5.000   6.000   6.235   7.000   9.000
```

## Ask a spatial question about the group contrast

Global Moran’s I below asks whether the *descriptive group-difference
map* is more spatially clustered than expected under value-label
permutation on the fixed DK graph. It does **not** test whether SCZ and
HC differ: that requires subject-level inference with the 200
independent subjects.

``` r
spatial_result <- ngeo_moran(
  dk$difference,
  weights,
  layer = "SCZ_minus_HC",
  permutations = 499,
  seed = 2026
)
spatial_result
#> <ngeo_global_stat>
#>   statistic: Moran's I
#>   estimate: 0.189658
#>   expectation: -0.0149254
#>   observations: 68
#>   permutations: 499
#>   p-value: 0.014
ngeo_inference_contract(spatial_result)
#> <ngeo_inference_contract> ngeo_global_stat
#>   estimand: global spatial autocorrelation statistic
#>   sampling unit: base elements
#>   null model: value-label permutation when requested
#>   metric: region_contiguity
#>   support: base elements
#>   uncertainty target: permutation distribution of the global statistic
```

The contract makes the distinction auditable: its sampling units are
parcels under a spatial randomization null. In a case-control model, the
sampling units are subjects and the exchangeability rule acts on
subjects, never vertices or parcels.

## Localize the descriptive pattern

``` r
local <- ngeo_local_moran(
  dk$difference, weights, layer = "SCZ_minus_HC",
  permutations = 499, seed = 2026, adjust = "BH",
  null_model = "conditional"
)
head(local[order(local$p.adjusted),
  c("element_id", "local_i", "p.adjusted", "cluster")], 10)
#> <ngeo_lisa>
#>   observations: 10
#>   layer:
#>   permutations:
```

Local Moran quadrants describe where similar contrast values cluster;
adjusted permutation evidence is required before calling a local pattern
significant. The brain map remains the primary orientation, while this
table supplies exact parcel-level values.

## What to report

> We simulated DK68 cortical thickness in 100 HC and 100 SCZ
> participants. A descriptive SCZ-minus-HC map was evaluated on a
> declared row-standardized DK adjacency graph using 499 value-label
> permutations. This spatial test characterizes clustering of the
> contrast map; it is not a subject-level test of diagnosis. Population
> inference must preserve subjects as the independent sampling unit and
> adjust the prespecified covariates.

Continue with [ROI/cohort data and
I/O](/en/tutorials/workflow-roi-cohort), [neighbors and
weights](/en/tutorials/neighbors-and-weights), [change of
support](/en/tutorials/change-of-support), or [multilayer subject-level
inference](/en/modules/multilayer-inference).
