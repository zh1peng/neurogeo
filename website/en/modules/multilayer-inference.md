---
title: "Subject-level multilayer inference in 100 HC and 100 SCZ"
outline: [2, 3]
editLink: false
sourceSha256: "7767810b2bc246993a33e4e238fedc5484c3961b4376cbbb94b28d2bfc9d43b5"
---

> **Translation status:** This page does not yet have a reviewed Simplified Chinese translation; the source-language version is shown.
**Edit source:** [Edit on GitHub](https://github.com/zh1peng/neurogeo/edit/main/vignettes/multilayer-inference.Rmd)


## Scientific question and data contract

For every subject, the same DK68 base carries cortical thickness and a
simulated myelin proxy. We ask whether their within-subject spatial
coupling differs between SCZ and HC after adjusting age, sex, and site.
One subject is one independent sampling unit; 68 parcels and two
modalities are repeated spatial measurements, not replication.

``` r
ngeo_tutorial_plot_dk(dk$difference,
                      title="Thickness: SCZ minus HC (mm)")
```

<div class="figure" style="text-align: center">

<img src="./multilayer-inference_files/figure-gfm/maps-1.png" alt="DK brain maps of simulated SCZ minus HC thickness and myelin proxy"  />
<p class="caption">

Both modalities are inspected on their shared cortical support before
coupling inference.
</p>

</div>

``` r
ngeo_tutorial_plot_dk(multi$myelin_difference,
                      title="Myelin proxy: SCZ minus HC (a.u.)")
```

<div class="figure" style="text-align: center">

<img src="./multilayer-inference_files/figure-gfm/maps-2.png" alt="DK brain maps of simulated SCZ minus HC thickness and myelin proxy"  />
<p class="caption">

Both modalities are inspected on their shared cortical support before
coupling inference.
</p>

</div>

``` r
dim(ngeo_values(x))
#> [1]  68 400
table(ngeo_layers(x)$feature, ngeo_layers(x)$group)
#>
#>              HC SCZ
#>   myelin    100 100
#>   thickness 100 100
index <- ngeo_validate_layers(
  x, required_layers=c("thickness", "myelin"), complete="error"
)
index
#> <ngeo_layer_index>
#>   unit: 200
#>   layers: 2
#>   observations: 400
#>   complete: TRUE
#>   index hash: bfa164d4fb301c1f3bef2ec64affc6b5ca72dffea9cf62bdfb73a7e6c30fbeb3
```

There are 68 rows and 400 layers: two aligned maps for each of 200
subjects. The layer index verifies that neither a subject nor a modality
is silently missing.

## Fix the spatial estimand before testing groups

``` r
weights <- ngeo_spatial_weights(
  x, method="region_contiguity", style="B"
)
basis <- ngeo_spatial_basis(
  x, weights, support="identity", n_modes=60
)
features <- ngeo_layer_coupling(
  x, index, basis=basis,
  bands=list(low_rank=1:8, mid_rank=9:20, high_rank=21:33),
  estimands=c("same_location", "spectral_coupling", "band_energy")
)
dim(features$values)
#> [1] 200  69
features$values[1:6, 1:6, drop=FALSE]
#>         same_location::thickness::myelin::none::whole_base::all
#> sub-001                                               0.7581317
#> sub-002                                               0.8361214
#> sub-003                                               0.8436612
#> sub-004                                               0.8415511
#> sub-005                                               0.8327421
#> sub-006                                               0.8619936
#>         band_energy_x::thickness::myelin::none::component_001::low_rank
#> sub-001                                                      0.17321496
#> sub-002                                                      0.11735904
#> sub-003                                                      0.06609750
#> sub-004                                                      0.10279415
#> sub-005                                                      0.09753059
#> sub-006                                                      0.14331719
#>         band_energy_y::thickness::myelin::none::component_001::low_rank
#> sub-001                                                       0.3697504
#> sub-002                                                       0.2175661
#> sub-003                                                       0.2955266
#> sub-004                                                       0.3052670
#> sub-005                                                       0.2474375
#> sub-006                                                       0.2771118
#>         spectral_cross_energy::thickness::myelin::none::component_001::low_rank
#> sub-001                                                               0.2072772
#> sub-002                                                               0.1517883
#> sub-003                                                               0.1186216
#> sub-004                                                               0.1634415
#> sub-005                                                               0.1479111
#> sub-006                                                               0.1863450
#>         spectral_coupling::thickness::myelin::none::component_001::low_rank
#> sub-001                                                           0.8190388
#> sub-002                                                           0.9499143
#> sub-003                                                           0.8487370
#> sub-004                                                           0.9226529
#> sub-005                                                           0.9521327
#> sub-006                                                           0.9350635
#>         retained_variance_x::thickness::myelin::none::component_001::low_rank
#> sub-001                                                                     1
#> sub-002                                                                     1
#> sub-003                                                                     1
#> sub-004                                                                     1
#> sub-005                                                                     1
#> sub-006                                                                     1
```

The graph basis depends only on the DK support and graph, not diagnosis
or values. Rank bands are retained graph modes; they are not
automatically millimetre wavelengths. `spectral_coupling` and marginal
`band_energy` remain separate endpoints so an energy change is not
mislabeled as coupling.

## Exchange whole subjects

``` r
design <- data.frame(
  unit_id=dk$design$subject_id,
  group=dk$design$group,
  age=dk$design$age,
  sex=dk$design$sex,
  site=dk$design$site
)
schedule <- ngeo_exchangeability(
  design$unit_id, scheme="within_block", blocks=design$site,
  permutations=199, seed=5001, unit_kind="subject"
)
result <- ngeo_group_test(
  features, design, model=~ age + sex + site + group, test="group",
  exchangeability=schedule, transform="auto", adjustment="maxT"
)
result$tests[, c(
  "estimand", "band", "coefficient", "partial_r2", "p_raw", "p_maxT"
)]
#>                                                                                       estimand
#> same_location::thickness::myelin::none::whole_base::all                          same_location
#> band_energy_x::thickness::myelin::none::component_001::low_rank                  band_energy_x
#> band_energy_y::thickness::myelin::none::component_001::low_rank                  band_energy_y
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank  spectral_cross_energy
#> spectral_coupling::thickness::myelin::none::component_001::low_rank          spectral_coupling
#> retained_variance_x::thickness::myelin::none::component_001::low_rank      retained_variance_x
#> retained_variance_y::thickness::myelin::none::component_001::low_rank      retained_variance_y
#> band_energy_x::thickness::myelin::none::component_001::mid_rank                  band_energy_x
#> band_energy_y::thickness::myelin::none::component_001::mid_rank                  band_energy_y
#> spectral_cross_energy::thickness::myelin::none::component_001::mid_rank  spectral_cross_energy
#> spectral_coupling::thickness::myelin::none::component_001::mid_rank          spectral_coupling
#> retained_variance_x::thickness::myelin::none::component_001::mid_rank      retained_variance_x
#> retained_variance_y::thickness::myelin::none::component_001::mid_rank      retained_variance_y
#> band_energy_x::thickness::myelin::none::component_001::high_rank                 band_energy_x
#> band_energy_y::thickness::myelin::none::component_001::high_rank                 band_energy_y
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank spectral_cross_energy
#> spectral_coupling::thickness::myelin::none::component_001::high_rank         spectral_coupling
#> retained_variance_x::thickness::myelin::none::component_001::high_rank     retained_variance_x
#> retained_variance_y::thickness::myelin::none::component_001::high_rank     retained_variance_y
#> absolute_energy::thickness::none::none::component_001::low_rank                absolute_energy
#> relative_energy::thickness::none::none::component_001::low_rank                relative_energy
#> absolute_energy::thickness::none::none::component_001::mid_rank                absolute_energy
#> relative_energy::thickness::none::none::component_001::mid_rank                relative_energy
#> absolute_energy::thickness::none::none::component_001::high_rank               absolute_energy
#> relative_energy::thickness::none::none::component_001::high_rank               relative_energy
#> retained_variance::thickness::none::none::component_001::retained            retained_variance
#> residual_energy::thickness::none::none::component_001::retained                residual_energy
#> absolute_energy::myelin::none::none::component_001::low_rank                   absolute_energy
#> relative_energy::myelin::none::none::component_001::low_rank                   relative_energy
#> absolute_energy::myelin::none::none::component_001::mid_rank                   absolute_energy
#> relative_energy::myelin::none::none::component_001::mid_rank                   relative_energy
#> absolute_energy::myelin::none::none::component_001::high_rank                  absolute_energy
#> relative_energy::myelin::none::none::component_001::high_rank                  relative_energy
#> retained_variance::myelin::none::none::component_001::retained               retained_variance
#> residual_energy::myelin::none::none::component_001::retained                   residual_energy
#> band_energy_x::thickness::myelin::none::component_002::low_rank                  band_energy_x
#> band_energy_y::thickness::myelin::none::component_002::low_rank                  band_energy_y
#> spectral_cross_energy::thickness::myelin::none::component_002::low_rank  spectral_cross_energy
#> spectral_coupling::thickness::myelin::none::component_002::low_rank          spectral_coupling
#> retained_variance_x::thickness::myelin::none::component_002::low_rank      retained_variance_x
#> retained_variance_y::thickness::myelin::none::component_002::low_rank      retained_variance_y
#> band_energy_x::thickness::myelin::none::component_002::mid_rank                  band_energy_x
#> band_energy_y::thickness::myelin::none::component_002::mid_rank                  band_energy_y
#> spectral_cross_energy::thickness::myelin::none::component_002::mid_rank  spectral_cross_energy
#> spectral_coupling::thickness::myelin::none::component_002::mid_rank          spectral_coupling
#> retained_variance_x::thickness::myelin::none::component_002::mid_rank      retained_variance_x
#> retained_variance_y::thickness::myelin::none::component_002::mid_rank      retained_variance_y
#> band_energy_x::thickness::myelin::none::component_002::high_rank                 band_energy_x
#> band_energy_y::thickness::myelin::none::component_002::high_rank                 band_energy_y
#> spectral_cross_energy::thickness::myelin::none::component_002::high_rank spectral_cross_energy
#> spectral_coupling::thickness::myelin::none::component_002::high_rank         spectral_coupling
#> retained_variance_x::thickness::myelin::none::component_002::high_rank     retained_variance_x
#> retained_variance_y::thickness::myelin::none::component_002::high_rank     retained_variance_y
#> absolute_energy::thickness::none::none::component_002::low_rank                absolute_energy
#> relative_energy::thickness::none::none::component_002::low_rank                relative_energy
#> absolute_energy::thickness::none::none::component_002::mid_rank                absolute_energy
#> relative_energy::thickness::none::none::component_002::mid_rank                relative_energy
#> absolute_energy::thickness::none::none::component_002::high_rank               absolute_energy
#> relative_energy::thickness::none::none::component_002::high_rank               relative_energy
#> retained_variance::thickness::none::none::component_002::retained            retained_variance
#> residual_energy::thickness::none::none::component_002::retained                residual_energy
#> absolute_energy::myelin::none::none::component_002::low_rank                   absolute_energy
#> relative_energy::myelin::none::none::component_002::low_rank                   relative_energy
#> absolute_energy::myelin::none::none::component_002::mid_rank                   absolute_energy
#> relative_energy::myelin::none::none::component_002::mid_rank                   relative_energy
#> absolute_energy::myelin::none::none::component_002::high_rank                  absolute_energy
#> relative_energy::myelin::none::none::component_002::high_rank                  relative_energy
#> retained_variance::myelin::none::none::component_002::retained               retained_variance
#> residual_energy::myelin::none::none::component_002::retained                   residual_energy
#>                                                                               band
#> same_location::thickness::myelin::none::whole_base::all                        all
#> band_energy_x::thickness::myelin::none::component_001::low_rank           low_rank
#> band_energy_y::thickness::myelin::none::component_001::low_rank           low_rank
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank   low_rank
#> spectral_coupling::thickness::myelin::none::component_001::low_rank       low_rank
#> retained_variance_x::thickness::myelin::none::component_001::low_rank     low_rank
#> retained_variance_y::thickness::myelin::none::component_001::low_rank     low_rank
#> band_energy_x::thickness::myelin::none::component_001::mid_rank           mid_rank
#> band_energy_y::thickness::myelin::none::component_001::mid_rank           mid_rank
#> spectral_cross_energy::thickness::myelin::none::component_001::mid_rank   mid_rank
#> spectral_coupling::thickness::myelin::none::component_001::mid_rank       mid_rank
#> retained_variance_x::thickness::myelin::none::component_001::mid_rank     mid_rank
#> retained_variance_y::thickness::myelin::none::component_001::mid_rank     mid_rank
#> band_energy_x::thickness::myelin::none::component_001::high_rank         high_rank
#> band_energy_y::thickness::myelin::none::component_001::high_rank         high_rank
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank high_rank
#> spectral_coupling::thickness::myelin::none::component_001::high_rank     high_rank
#> retained_variance_x::thickness::myelin::none::component_001::high_rank   high_rank
#> retained_variance_y::thickness::myelin::none::component_001::high_rank   high_rank
#> absolute_energy::thickness::none::none::component_001::low_rank           low_rank
#> relative_energy::thickness::none::none::component_001::low_rank           low_rank
#> absolute_energy::thickness::none::none::component_001::mid_rank           mid_rank
#> relative_energy::thickness::none::none::component_001::mid_rank           mid_rank
#> absolute_energy::thickness::none::none::component_001::high_rank         high_rank
#> relative_energy::thickness::none::none::component_001::high_rank         high_rank
#> retained_variance::thickness::none::none::component_001::retained         retained
#> residual_energy::thickness::none::none::component_001::retained           retained
#> absolute_energy::myelin::none::none::component_001::low_rank              low_rank
#> relative_energy::myelin::none::none::component_001::low_rank              low_rank
#> absolute_energy::myelin::none::none::component_001::mid_rank              mid_rank
#> relative_energy::myelin::none::none::component_001::mid_rank              mid_rank
#> absolute_energy::myelin::none::none::component_001::high_rank            high_rank
#> relative_energy::myelin::none::none::component_001::high_rank            high_rank
#> retained_variance::myelin::none::none::component_001::retained            retained
#> residual_energy::myelin::none::none::component_001::retained              retained
#> band_energy_x::thickness::myelin::none::component_002::low_rank           low_rank
#> band_energy_y::thickness::myelin::none::component_002::low_rank           low_rank
#> spectral_cross_energy::thickness::myelin::none::component_002::low_rank   low_rank
#> spectral_coupling::thickness::myelin::none::component_002::low_rank       low_rank
#> retained_variance_x::thickness::myelin::none::component_002::low_rank     low_rank
#> retained_variance_y::thickness::myelin::none::component_002::low_rank     low_rank
#> band_energy_x::thickness::myelin::none::component_002::mid_rank           mid_rank
#> band_energy_y::thickness::myelin::none::component_002::mid_rank           mid_rank
#> spectral_cross_energy::thickness::myelin::none::component_002::mid_rank   mid_rank
#> spectral_coupling::thickness::myelin::none::component_002::mid_rank       mid_rank
#> retained_variance_x::thickness::myelin::none::component_002::mid_rank     mid_rank
#> retained_variance_y::thickness::myelin::none::component_002::mid_rank     mid_rank
#> band_energy_x::thickness::myelin::none::component_002::high_rank         high_rank
#> band_energy_y::thickness::myelin::none::component_002::high_rank         high_rank
#> spectral_cross_energy::thickness::myelin::none::component_002::high_rank high_rank
#> spectral_coupling::thickness::myelin::none::component_002::high_rank     high_rank
#> retained_variance_x::thickness::myelin::none::component_002::high_rank   high_rank
#> retained_variance_y::thickness::myelin::none::component_002::high_rank   high_rank
#> absolute_energy::thickness::none::none::component_002::low_rank           low_rank
#> relative_energy::thickness::none::none::component_002::low_rank           low_rank
#> absolute_energy::thickness::none::none::component_002::mid_rank           mid_rank
#> relative_energy::thickness::none::none::component_002::mid_rank           mid_rank
#> absolute_energy::thickness::none::none::component_002::high_rank         high_rank
#> relative_energy::thickness::none::none::component_002::high_rank         high_rank
#> retained_variance::thickness::none::none::component_002::retained         retained
#> residual_energy::thickness::none::none::component_002::retained           retained
#> absolute_energy::myelin::none::none::component_002::low_rank              low_rank
#> relative_energy::myelin::none::none::component_002::low_rank              low_rank
#> absolute_energy::myelin::none::none::component_002::mid_rank              mid_rank
#> relative_energy::myelin::none::none::component_002::mid_rank              mid_rank
#> absolute_energy::myelin::none::none::component_002::high_rank            high_rank
#> relative_energy::myelin::none::none::component_002::high_rank            high_rank
#> retained_variance::myelin::none::none::component_002::retained            retained
#> residual_energy::myelin::none::none::component_002::retained              retained
#>                                                                            coefficient
#> same_location::thickness::myelin::none::whole_base::all                  -1.712284e-01
#> band_energy_x::thickness::myelin::none::component_001::low_rank           6.789404e-03
#> band_energy_y::thickness::myelin::none::component_001::low_rank          -7.974139e-02
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank  -2.502451e-02
#> spectral_coupling::thickness::myelin::none::component_001::low_rank      -1.752193e-01
#> retained_variance_x::thickness::myelin::none::component_001::low_rank     4.026929e-16
#> retained_variance_y::thickness::myelin::none::component_001::low_rank     4.337069e-16
#> band_energy_x::thickness::myelin::none::component_001::mid_rank          -2.293473e-03
#> band_energy_y::thickness::myelin::none::component_001::mid_rank          -1.968010e-02
#> spectral_cross_energy::thickness::myelin::none::component_001::mid_rank  -1.496885e-02
#> spectral_coupling::thickness::myelin::none::component_001::mid_rank      -2.546908e-01
#> retained_variance_x::thickness::myelin::none::component_001::mid_rank     4.026929e-16
#> retained_variance_y::thickness::myelin::none::component_001::mid_rank     4.337069e-16
#> band_energy_x::thickness::myelin::none::component_001::high_rank         -1.452173e-03
#> band_energy_y::thickness::myelin::none::component_001::high_rank         -1.384606e-02
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank -1.141279e-02
#> spectral_coupling::thickness::myelin::none::component_001::high_rank     -2.171436e-01
#> retained_variance_x::thickness::myelin::none::component_001::high_rank    4.026929e-16
#> retained_variance_y::thickness::myelin::none::component_001::high_rank    4.337069e-16
#> absolute_energy::thickness::none::none::component_001::low_rank           6.789404e-03
#> relative_energy::thickness::none::none::component_001::low_rank           2.813180e-02
#> absolute_energy::thickness::none::none::component_001::mid_rank          -2.293473e-03
#> relative_energy::thickness::none::none::component_001::mid_rank          -1.667651e-02
#> absolute_energy::thickness::none::none::component_001::high_rank         -1.452173e-03
#> relative_energy::thickness::none::none::component_001::high_rank         -1.145529e-02
#> retained_variance::thickness::none::none::component_001::retained         4.026929e-16
#> residual_energy::thickness::none::none::component_001::retained           8.195667e-19
#> absolute_energy::myelin::none::none::component_001::low_rank             -7.974139e-02
#> relative_energy::myelin::none::none::component_001::low_rank             -1.734337e-02
#> absolute_energy::myelin::none::none::component_001::mid_rank             -1.968010e-02
#> relative_energy::myelin::none::none::component_001::mid_rank             -4.396882e-04
#> absolute_energy::myelin::none::none::component_001::high_rank            -1.384606e-02
#> relative_energy::myelin::none::none::component_001::high_rank             1.778306e-02
#> retained_variance::myelin::none::none::component_001::retained            4.337069e-16
#> residual_energy::myelin::none::none::component_001::retained             -2.657090e-18
#> band_energy_x::thickness::myelin::none::component_002::low_rank          -4.826282e-03
#> band_energy_y::thickness::myelin::none::component_002::low_rank          -8.845102e-02
#> spectral_cross_energy::thickness::myelin::none::component_002::low_rank  -3.542579e-02
#> spectral_coupling::thickness::myelin::none::component_002::low_rank      -1.932329e-01
#> retained_variance_x::thickness::myelin::none::component_002::low_rank     4.181574e-16
#> retained_variance_y::thickness::myelin::none::component_002::low_rank     1.777439e-16
#> band_energy_x::thickness::myelin::none::component_002::mid_rank           1.300805e-03
#> band_energy_y::thickness::myelin::none::component_002::mid_rank          -1.465591e-02
#> spectral_cross_energy::thickness::myelin::none::component_002::mid_rank  -7.442833e-03
#> spectral_coupling::thickness::myelin::none::component_002::mid_rank      -1.526334e-01
#> retained_variance_x::thickness::myelin::none::component_002::mid_rank     4.181574e-16
#> retained_variance_y::thickness::myelin::none::component_002::mid_rank     1.777439e-16
#> band_energy_x::thickness::myelin::none::component_002::high_rank         -9.474543e-04
#> band_energy_y::thickness::myelin::none::component_002::high_rank         -1.808352e-02
#> spectral_cross_energy::thickness::myelin::none::component_002::high_rank -1.138419e-02
#> spectral_coupling::thickness::myelin::none::component_002::high_rank     -1.970272e-01
#> retained_variance_x::thickness::myelin::none::component_002::high_rank    4.181574e-16
#> retained_variance_y::thickness::myelin::none::component_002::high_rank    1.777439e-16
#> absolute_energy::thickness::none::none::component_002::low_rank          -4.826282e-03
#> relative_energy::thickness::none::none::component_002::low_rank          -1.048412e-02
#> absolute_energy::thickness::none::none::component_002::mid_rank           1.300805e-03
#> relative_energy::thickness::none::none::component_002::mid_rank           1.286863e-02
#> absolute_energy::thickness::none::none::component_002::high_rank         -9.474543e-04
#> relative_energy::thickness::none::none::component_002::high_rank         -2.384515e-03
#> retained_variance::thickness::none::none::component_002::retained         4.181574e-16
#> residual_energy::thickness::none::none::component_002::retained          -1.046244e-17
#> absolute_energy::myelin::none::none::component_002::low_rank             -8.845102e-02
#> relative_energy::myelin::none::none::component_002::low_rank             -2.326506e-02
#> absolute_energy::myelin::none::none::component_002::mid_rank             -1.465591e-02
#> relative_energy::myelin::none::none::component_002::mid_rank              1.249800e-02
#> absolute_energy::myelin::none::none::component_002::high_rank            -1.808352e-02
#> relative_energy::myelin::none::none::component_002::high_rank             1.076706e-02
#> retained_variance::myelin::none::none::component_002::retained            1.777439e-16
#> residual_energy::myelin::none::none::component_002::retained             -2.593391e-18
#>                                                                            partial_r2
#> same_location::thickness::myelin::none::whole_base::all                  4.153978e-01
#> band_energy_x::thickness::myelin::none::component_001::low_rank          7.092988e-03
#> band_energy_y::thickness::myelin::none::component_001::low_rank          3.521419e-01
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank  8.400691e-02
#> spectral_coupling::thickness::myelin::none::component_001::low_rank      7.351916e-02
#> retained_variance_x::thickness::myelin::none::component_001::low_rank    5.065950e-03
#> retained_variance_y::thickness::myelin::none::component_001::low_rank    3.987126e-03
#> band_energy_x::thickness::myelin::none::component_001::mid_rank          3.297436e-03
#> band_energy_y::thickness::myelin::none::component_001::mid_rank          1.295016e-01
#> spectral_cross_energy::thickness::myelin::none::component_001::mid_rank  1.328803e-01
#> spectral_coupling::thickness::myelin::none::component_001::mid_rank      1.478896e-01
#> retained_variance_x::thickness::myelin::none::component_001::mid_rank    5.065950e-03
#> retained_variance_y::thickness::myelin::none::component_001::mid_rank    3.987126e-03
#> band_energy_x::thickness::myelin::none::component_001::high_rank         1.990539e-03
#> band_energy_y::thickness::myelin::none::component_001::high_rank         7.152914e-02
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank 9.961046e-02
#> spectral_coupling::thickness::myelin::none::component_001::high_rank     1.142896e-01
#> retained_variance_x::thickness::myelin::none::component_001::high_rank   5.065950e-03
#> retained_variance_y::thickness::myelin::none::component_001::high_rank   3.987126e-03
#> absolute_energy::thickness::none::none::component_001::low_rank          7.092988e-03
#> relative_energy::thickness::none::none::component_001::low_rank          1.588836e-02
#> absolute_energy::thickness::none::none::component_001::mid_rank          3.297436e-03
#> relative_energy::thickness::none::none::component_001::mid_rank          7.882805e-03
#> absolute_energy::thickness::none::none::component_001::high_rank         1.990539e-03
#> relative_energy::thickness::none::none::component_001::high_rank         6.627515e-03
#> retained_variance::thickness::none::none::component_001::retained        5.065950e-03
#> residual_energy::thickness::none::none::component_001::retained          1.181376e-04
#> absolute_energy::myelin::none::none::component_001::low_rank             3.521419e-01
#> relative_energy::myelin::none::none::component_001::low_rank             1.397632e-02
#> absolute_energy::myelin::none::none::component_001::mid_rank             1.295016e-01
#> relative_energy::myelin::none::none::component_001::mid_rank             1.240132e-05
#> absolute_energy::myelin::none::none::component_001::high_rank            7.152914e-02
#> relative_energy::myelin::none::none::component_001::high_rank            2.458278e-02
#> retained_variance::myelin::none::none::component_001::retained           3.987126e-03
#> residual_energy::myelin::none::none::component_001::retained             9.051888e-03
#> band_energy_x::thickness::myelin::none::component_002::low_rank          3.058815e-03
#> band_energy_y::thickness::myelin::none::component_002::low_rank          4.141207e-01
#> spectral_cross_energy::thickness::myelin::none::component_002::low_rank  1.372023e-01
#> spectral_coupling::thickness::myelin::none::component_002::low_rank      7.469590e-02
#> retained_variance_x::thickness::myelin::none::component_002::low_rank    9.740305e-03
#> retained_variance_y::thickness::myelin::none::component_002::low_rank    2.489612e-03
#> band_energy_x::thickness::myelin::none::component_002::mid_rank          1.209878e-03
#> band_energy_y::thickness::myelin::none::component_002::mid_rank          1.128970e-01
#> spectral_cross_energy::thickness::myelin::none::component_002::mid_rank  5.386861e-02
#> spectral_coupling::thickness::myelin::none::component_002::mid_rank      6.431016e-02
#> retained_variance_x::thickness::myelin::none::component_002::mid_rank    9.740305e-03
#> retained_variance_y::thickness::myelin::none::component_002::mid_rank    2.489612e-03
#> band_energy_x::thickness::myelin::none::component_002::high_rank         7.776114e-04
#> band_energy_y::thickness::myelin::none::component_002::high_rank         1.221932e-01
#> spectral_cross_energy::thickness::myelin::none::component_002::high_rank 1.065726e-01
#> spectral_coupling::thickness::myelin::none::component_002::high_rank     1.131727e-01
#> retained_variance_x::thickness::myelin::none::component_002::high_rank   9.740305e-03
#> retained_variance_y::thickness::myelin::none::component_002::high_rank   2.489612e-03
#> absolute_energy::thickness::none::none::component_002::low_rank          3.058815e-03
#> relative_energy::thickness::none::none::component_002::low_rank          2.519725e-03
#> absolute_energy::thickness::none::none::component_002::mid_rank          1.209878e-03
#> relative_energy::thickness::none::none::component_002::mid_rank          6.683944e-03
#> absolute_energy::thickness::none::none::component_002::high_rank         7.776114e-04
#> relative_energy::thickness::none::none::component_002::high_rank         2.049978e-04
#> retained_variance::thickness::none::none::component_002::retained        9.740305e-03
#> residual_energy::thickness::none::none::component_002::retained          3.461489e-03
#> absolute_energy::myelin::none::none::component_002::low_rank             4.141207e-01
#> relative_energy::myelin::none::none::component_002::low_rank             2.963264e-02
#> absolute_energy::myelin::none::none::component_002::mid_rank             1.128970e-01
#> relative_energy::myelin::none::none::component_002::mid_rank             1.789017e-02
#> absolute_energy::myelin::none::none::component_002::high_rank            1.221932e-01
#> relative_energy::myelin::none::none::component_002::high_rank            1.017519e-02
#> retained_variance::myelin::none::none::component_002::retained           2.489612e-03
#> residual_energy::myelin::none::none::component_002::retained             8.412822e-05
#>                                                                          p_raw
#> same_location::thickness::myelin::none::whole_base::all                  0.005
#> band_energy_x::thickness::myelin::none::component_001::low_rank          0.265
#> band_energy_y::thickness::myelin::none::component_001::low_rank          0.005
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank  0.005
#> spectral_coupling::thickness::myelin::none::component_001::low_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_001::low_rank    0.430
#> retained_variance_y::thickness::myelin::none::component_001::low_rank    0.520
#> band_energy_x::thickness::myelin::none::component_001::mid_rank          0.395
#> band_energy_y::thickness::myelin::none::component_001::mid_rank          0.005
#> spectral_cross_energy::thickness::myelin::none::component_001::mid_rank  0.005
#> spectral_coupling::thickness::myelin::none::component_001::mid_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_001::mid_rank    0.430
#> retained_variance_y::thickness::myelin::none::component_001::mid_rank    0.520
#> band_energy_x::thickness::myelin::none::component_001::high_rank         0.595
#> band_energy_y::thickness::myelin::none::component_001::high_rank         0.005
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank 0.005
#> spectral_coupling::thickness::myelin::none::component_001::high_rank     0.005
#> retained_variance_x::thickness::myelin::none::component_001::high_rank   0.430
#> retained_variance_y::thickness::myelin::none::component_001::high_rank   0.520
#> absolute_energy::thickness::none::none::component_001::low_rank          0.265
#> relative_energy::thickness::none::none::component_001::low_rank          0.095
#> absolute_energy::thickness::none::none::component_001::mid_rank          0.395
#> relative_energy::thickness::none::none::component_001::mid_rank          0.200
#> absolute_energy::thickness::none::none::component_001::high_rank         0.595
#> relative_energy::thickness::none::none::component_001::high_rank         0.310
#> retained_variance::thickness::none::none::component_001::retained        0.430
#> residual_energy::thickness::none::none::component_001::retained          0.885
#> absolute_energy::myelin::none::none::component_001::low_rank             0.005
#> relative_energy::myelin::none::none::component_001::low_rank             0.115
#> absolute_energy::myelin::none::none::component_001::mid_rank             0.005
#> relative_energy::myelin::none::none::component_001::mid_rank             0.975
#> absolute_energy::myelin::none::none::component_001::high_rank            0.005
#> relative_energy::myelin::none::none::component_001::high_rank            0.025
#> retained_variance::myelin::none::none::component_001::retained           0.520
#> residual_energy::myelin::none::none::component_001::retained             0.245
#> band_energy_x::thickness::myelin::none::component_002::low_rank          0.500
#> band_energy_y::thickness::myelin::none::component_002::low_rank          0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::low_rank  0.005
#> spectral_coupling::thickness::myelin::none::component_002::low_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_002::low_rank    0.160
#> retained_variance_y::thickness::myelin::none::component_002::low_rank    0.620
#> band_energy_x::thickness::myelin::none::component_002::mid_rank          0.645
#> band_energy_y::thickness::myelin::none::component_002::mid_rank          0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::mid_rank  0.010
#> spectral_coupling::thickness::myelin::none::component_002::mid_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_002::mid_rank    0.160
#> retained_variance_y::thickness::myelin::none::component_002::mid_rank    0.620
#> band_energy_x::thickness::myelin::none::component_002::high_rank         0.700
#> band_energy_y::thickness::myelin::none::component_002::high_rank         0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::high_rank 0.005
#> spectral_coupling::thickness::myelin::none::component_002::high_rank     0.005
#> retained_variance_x::thickness::myelin::none::component_002::high_rank   0.160
#> retained_variance_y::thickness::myelin::none::component_002::high_rank   0.620
#> absolute_energy::thickness::none::none::component_002::low_rank          0.500
#> relative_energy::thickness::none::none::component_002::low_rank          0.565
#> absolute_energy::thickness::none::none::component_002::mid_rank          0.645
#> relative_energy::thickness::none::none::component_002::mid_rank          0.320
#> absolute_energy::thickness::none::none::component_002::high_rank         0.700
#> relative_energy::thickness::none::none::component_002::high_rank         0.840
#> retained_variance::thickness::none::none::component_002::retained        0.160
#> residual_energy::thickness::none::none::component_002::retained          0.385
#> absolute_energy::myelin::none::none::component_002::low_rank             0.005
#> relative_energy::myelin::none::none::component_002::low_rank             0.015
#> absolute_energy::myelin::none::none::component_002::mid_rank             0.005
#> relative_energy::myelin::none::none::component_002::mid_rank             0.055
#> absolute_energy::myelin::none::none::component_002::high_rank            0.005
#> relative_energy::myelin::none::none::component_002::high_rank            0.195
#> retained_variance::myelin::none::none::component_002::retained           0.620
#> residual_energy::myelin::none::none::component_002::retained             0.910
#>                                                                          p_maxT
#> same_location::thickness::myelin::none::whole_base::all                   0.005
#> band_energy_x::thickness::myelin::none::component_001::low_rank           1.000
#> band_energy_y::thickness::myelin::none::component_001::low_rank           0.005
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank   0.005
#> spectral_coupling::thickness::myelin::none::component_001::low_rank       0.010
#> retained_variance_x::thickness::myelin::none::component_001::low_rank     1.000
#> retained_variance_y::thickness::myelin::none::component_001::low_rank     1.000
#> band_energy_x::thickness::myelin::none::component_001::mid_rank           1.000
#> band_energy_y::thickness::myelin::none::component_001::mid_rank           0.005
#> spectral_cross_energy::thickness::myelin::none::component_001::mid_rank   0.005
#> spectral_coupling::thickness::myelin::none::component_001::mid_rank       0.005
#> retained_variance_x::thickness::myelin::none::component_001::mid_rank     1.000
#> retained_variance_y::thickness::myelin::none::component_001::mid_rank     1.000
#> band_energy_x::thickness::myelin::none::component_001::high_rank          1.000
#> band_energy_y::thickness::myelin::none::component_001::high_rank          0.010
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank  0.005
#> spectral_coupling::thickness::myelin::none::component_001::high_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_001::high_rank    1.000
#> retained_variance_y::thickness::myelin::none::component_001::high_rank    1.000
#> absolute_energy::thickness::none::none::component_001::low_rank           1.000
#> relative_energy::thickness::none::none::component_001::low_rank           0.890
#> absolute_energy::thickness::none::none::component_001::mid_rank           1.000
#> relative_energy::thickness::none::none::component_001::mid_rank           1.000
#> absolute_energy::thickness::none::none::component_001::high_rank          1.000
#> relative_energy::thickness::none::none::component_001::high_rank          1.000
#> retained_variance::thickness::none::none::component_001::retained         1.000
#> residual_energy::thickness::none::none::component_001::retained           1.000
#> absolute_energy::myelin::none::none::component_001::low_rank              0.005
#> relative_energy::myelin::none::none::component_001::low_rank              0.955
#> absolute_energy::myelin::none::none::component_001::mid_rank              0.005
#> relative_energy::myelin::none::none::component_001::mid_rank              1.000
#> absolute_energy::myelin::none::none::component_001::high_rank             0.010
#> relative_energy::myelin::none::none::component_001::high_rank             0.555
#> retained_variance::myelin::none::none::component_001::retained            1.000
#> residual_energy::myelin::none::none::component_001::retained              1.000
#> band_energy_x::thickness::myelin::none::component_002::low_rank           1.000
#> band_energy_y::thickness::myelin::none::component_002::low_rank           0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::low_rank   0.005
#> spectral_coupling::thickness::myelin::none::component_002::low_rank       0.010
#> retained_variance_x::thickness::myelin::none::component_002::low_rank     1.000
#> retained_variance_y::thickness::myelin::none::component_002::low_rank     1.000
#> band_energy_x::thickness::myelin::none::component_002::mid_rank           1.000
#> band_energy_y::thickness::myelin::none::component_002::mid_rank           0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::mid_rank   0.040
#> spectral_coupling::thickness::myelin::none::component_002::mid_rank       0.015
#> retained_variance_x::thickness::myelin::none::component_002::mid_rank     1.000
#> retained_variance_y::thickness::myelin::none::component_002::mid_rank     1.000
#> band_energy_x::thickness::myelin::none::component_002::high_rank          1.000
#> band_energy_y::thickness::myelin::none::component_002::high_rank          0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::high_rank  0.005
#> spectral_coupling::thickness::myelin::none::component_002::high_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_002::high_rank    1.000
#> retained_variance_y::thickness::myelin::none::component_002::high_rank    1.000
#> absolute_energy::thickness::none::none::component_002::low_rank           1.000
#> relative_energy::thickness::none::none::component_002::low_rank           1.000
#> absolute_energy::thickness::none::none::component_002::mid_rank           1.000
#> relative_energy::thickness::none::none::component_002::mid_rank           1.000
#> absolute_energy::thickness::none::none::component_002::high_rank          1.000
#> relative_energy::thickness::none::none::component_002::high_rank          1.000
#> retained_variance::thickness::none::none::component_002::retained         1.000
#> residual_energy::thickness::none::none::component_002::retained           1.000
#> absolute_energy::myelin::none::none::component_002::low_rank              0.005
#> relative_energy::myelin::none::none::component_002::low_rank              0.370
#> absolute_energy::myelin::none::none::component_002::mid_rank              0.005
#> relative_energy::myelin::none::none::component_002::mid_rank              0.840
#> absolute_energy::myelin::none::none::component_002::high_rank             0.005
#> relative_energy::myelin::none::none::component_002::high_rank             1.000
#> retained_variance::myelin::none::none::component_002::retained            1.000
#> residual_energy::myelin::none::none::component_002::retained              1.000
result$omnibus
#>   omnibus  statistic p_value
#> 1     max   11.77116   0.005
#> 2  sum_sq 1134.49006   0.005
```

One schedule moves complete subject records within site and is reused
for all bands and endpoints. It never permutes parcels. `p_maxT`
controls the declared endpoint family; the omnibus test asks whether any
endpoint in that family differs by group under the same schedule.

## Extend to a support family

Run the feature construction independently on registered DK68,
Schaefer100/200/300, or another prespecified atlas, then pass the named
feature list to one `ngeo_group_test()` call. A common subject schedule
and full-family max-T adjustment are essential. Atlas dispersion
describes sensitivity; it does not prove parcellation invariance.

The simulator’s Schaefer-sized boundaries are for operator teaching
only. Scientific cross-atlas claims require the published labels
registered to the same subjects and documented physical scale.
