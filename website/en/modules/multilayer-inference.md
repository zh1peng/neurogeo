---
title: "Subject-level multilayer inference in 100 HC and 100 SCZ"
outline: [2, 3]
editLink: false
sourceSha256: "9030afb1b60911d0ee967f41cdcbf3d2b8937ae4745f561be47e8b2c6d078748"
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
thickness_values <- setNames(
  ngeo_values(dk$difference)[, 1L],
  ngeo_base_elements(dk$difference)$region_id
)
thickness_limit <- max(abs(thickness_values), na.rm=TRUE)
thickness_map <- ngeo_cortical_map(
  dk$surface,
  values=thickness_values,
  chart="flat",
  atlas=dk$atlas,
  underlay=dk$underlay,
  underlay_palette="Grays",
  overlay_alpha=0.82,
  palette="Blue-Red 3",
  limits=c(-thickness_limit, thickness_limit),
  na_color=NA_character_,
  atlas_coverage="auto"
)
plot(
  thickness_map,
  main="Thickness: SCZ minus HC (mm)",
  boundary_color=grDevices::adjustcolor("white", 0.55),
  boundary_lwd=0.25,
  outline_lwd=1.1
)
```

<div class="figure" style="text-align: center">

<img src="./multilayer-inference_files/figure-gfm/maps-1.png" alt="DK brain maps of simulated SCZ minus HC thickness and myelin proxy"  />
<p class="caption">

Both modalities are inspected on their shared cortical support before
coupling inference.
</p>

</div>

``` r

myelin_values <- setNames(
  ngeo_values(multi$myelin_difference)[, 1L],
  ngeo_base_elements(multi$myelin_difference)$region_id
)
myelin_limit <- max(abs(myelin_values), na.rm=TRUE)
myelin_map <- ngeo_cortical_map(
  dk$surface,
  values=myelin_values,
  chart="flat",
  atlas=dk$atlas,
  underlay=dk$underlay,
  underlay_palette="Grays",
  overlay_alpha=0.82,
  palette="Blue-Red 3",
  limits=c(-myelin_limit, myelin_limit),
  na_color=NA_character_,
  atlas_coverage="auto"
)
plot(
  myelin_map,
  main="Myelin proxy: SCZ minus HC (a.u.)",
  boundary_color=grDevices::adjustcolor("white", 0.55),
  boundary_lwd=0.25,
  outline_lwd=1.1
)
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
#>   index hash: e6072dd1c501b22171dda1dd9d8ae608c8f8b6feaac58587dea0c4e0775e4d6f
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
#> sub-001                                               0.7461137
#> sub-002                                               0.8171695
#> sub-003                                               0.8716993
#> sub-004                                               0.8502657
#> sub-005                                               0.8239913
#> sub-006                                               0.8740804
#>         band_energy_x::thickness::myelin::none::component_001::low_rank
#> sub-001                                                      0.10724094
#> sub-002                                                      0.05188418
#> sub-003                                                      0.03525282
#> sub-004                                                      0.05095546
#> sub-005                                                      0.09929737
#> sub-006                                                      0.07825277
#>         band_energy_y::thickness::myelin::none::component_001::low_rank
#> sub-001                                                       0.0930335
#> sub-002                                                       0.1941668
#> sub-003                                                       0.1415373
#> sub-004                                                       0.1605296
#> sub-005                                                       0.1513407
#> sub-006                                                       0.1691022
#>         spectral_cross_energy::thickness::myelin::none::component_001::low_rank
#> sub-001                                                              0.08185055
#> sub-002                                                              0.07768153
#> sub-003                                                              0.05690657
#> sub-004                                                              0.08000201
#> sub-005                                                              0.11322890
#> sub-006                                                              0.11051024
#>         spectral_coupling::thickness::myelin::none::component_001::low_rank
#> sub-001                                                           0.8194484
#> sub-002                                                           0.7739498
#> sub-003                                                           0.8056197
#> sub-004                                                           0.8845612
#> sub-005                                                           0.9236566
#> sub-006                                                           0.9606783
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
#> same_location::thickness::myelin::none::whole_base::all                  -3.483872e-01
#> band_energy_x::thickness::myelin::none::component_001::low_rank           7.911903e-02
#> band_energy_y::thickness::myelin::none::component_001::low_rank          -4.595830e-02
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank  -2.053207e-03
#> spectral_coupling::thickness::myelin::none::component_001::low_rank      -4.643852e-01
#> retained_variance_x::thickness::myelin::none::component_001::low_rank     3.297805e-16
#> retained_variance_y::thickness::myelin::none::component_001::low_rank     3.935090e-16
#> band_energy_x::thickness::myelin::none::component_001::mid_rank          -4.192498e-03
#> band_energy_y::thickness::myelin::none::component_001::mid_rank          -3.821971e-02
#> spectral_cross_energy::thickness::myelin::none::component_001::mid_rank  -2.776312e-02
#> spectral_coupling::thickness::myelin::none::component_001::mid_rank      -3.656456e-01
#> retained_variance_x::thickness::myelin::none::component_001::mid_rank     3.297805e-16
#> retained_variance_y::thickness::myelin::none::component_001::mid_rank     3.935090e-16
#> band_energy_x::thickness::myelin::none::component_001::high_rank          1.179336e-02
#> band_energy_y::thickness::myelin::none::component_001::high_rank         -4.331710e-02
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank -1.406494e-02
#> spectral_coupling::thickness::myelin::none::component_001::high_rank     -1.889127e-01
#> retained_variance_x::thickness::myelin::none::component_001::high_rank    3.297805e-16
#> retained_variance_y::thickness::myelin::none::component_001::high_rank    3.935090e-16
#> absolute_energy::thickness::none::none::component_001::low_rank           7.911903e-02
#> relative_energy::thickness::none::none::component_001::low_rank           1.601657e-01
#> absolute_energy::thickness::none::none::component_001::mid_rank          -4.192498e-03
#> relative_energy::thickness::none::none::component_001::mid_rank          -1.025381e-01
#> absolute_energy::thickness::none::none::component_001::high_rank          1.179336e-02
#> relative_energy::thickness::none::none::component_001::high_rank         -5.762757e-02
#> retained_variance::thickness::none::none::component_001::retained         3.297805e-16
#> residual_energy::thickness::none::none::component_001::retained          -1.843163e-17
#> absolute_energy::myelin::none::none::component_001::low_rank             -4.595830e-02
#> relative_energy::myelin::none::none::component_001::low_rank             -2.060818e-02
#> absolute_energy::myelin::none::none::component_001::mid_rank             -3.821971e-02
#> relative_energy::myelin::none::none::component_001::mid_rank              1.176157e-02
#> absolute_energy::myelin::none::none::component_001::high_rank            -4.331710e-02
#> relative_energy::myelin::none::none::component_001::high_rank             8.846612e-03
#> retained_variance::myelin::none::none::component_001::retained            3.935090e-16
#> residual_energy::myelin::none::none::component_001::retained             -3.650621e-18
#> band_energy_x::thickness::myelin::none::component_002::low_rank           5.732228e-02
#> band_energy_y::thickness::myelin::none::component_002::low_rank          -5.487609e-02
#> spectral_cross_energy::thickness::myelin::none::component_002::low_rank  -1.933690e-02
#> spectral_coupling::thickness::myelin::none::component_002::low_rank      -5.790350e-01
#> retained_variance_x::thickness::myelin::none::component_002::low_rank    -1.854331e-15
#> retained_variance_y::thickness::myelin::none::component_002::low_rank    -4.864842e-16
#> band_energy_x::thickness::myelin::none::component_002::mid_rank           4.086086e-03
#> band_energy_y::thickness::myelin::none::component_002::mid_rank          -3.123685e-02
#> spectral_cross_energy::thickness::myelin::none::component_002::mid_rank  -1.473460e-02
#> spectral_coupling::thickness::myelin::none::component_002::mid_rank      -1.997431e-01
#> retained_variance_x::thickness::myelin::none::component_002::mid_rank    -1.854331e-15
#> retained_variance_y::thickness::myelin::none::component_002::mid_rank    -4.864842e-16
#> band_energy_x::thickness::myelin::none::component_002::high_rank          2.802990e-03
#> band_energy_y::thickness::myelin::none::component_002::high_rank         -5.116500e-02
#> spectral_cross_energy::thickness::myelin::none::component_002::high_rank -2.471686e-02
#> spectral_coupling::thickness::myelin::none::component_002::high_rank     -2.720141e-01
#> retained_variance_x::thickness::myelin::none::component_002::high_rank   -1.854331e-15
#> retained_variance_y::thickness::myelin::none::component_002::high_rank   -4.864842e-16
#> absolute_energy::thickness::none::none::component_002::low_rank           5.732228e-02
#> relative_energy::thickness::none::none::component_002::low_rank           1.194350e-01
#> absolute_energy::thickness::none::none::component_002::mid_rank           4.086086e-03
#> relative_energy::thickness::none::none::component_002::mid_rank          -5.052101e-02
#> absolute_energy::thickness::none::none::component_002::high_rank          2.802990e-03
#> relative_energy::thickness::none::none::component_002::high_rank         -6.891398e-02
#> retained_variance::thickness::none::none::component_002::retained        -1.854331e-15
#> residual_energy::thickness::none::none::component_002::retained           3.227213e-16
#> absolute_energy::myelin::none::none::component_002::low_rank             -5.487609e-02
#> relative_energy::myelin::none::none::component_002::low_rank             -1.998008e-02
#> absolute_energy::myelin::none::none::component_002::mid_rank             -3.123685e-02
#> relative_energy::myelin::none::none::component_002::mid_rank              2.302782e-02
#> absolute_energy::myelin::none::none::component_002::high_rank            -5.116500e-02
#> relative_energy::myelin::none::none::component_002::high_rank            -3.047746e-03
#> retained_variance::myelin::none::none::component_002::retained           -4.864842e-16
#> residual_energy::myelin::none::none::component_002::retained              7.701050e-17
#>                                                                            partial_r2
#> same_location::thickness::myelin::none::whole_base::all                  0.7480944931
#> band_energy_x::thickness::myelin::none::component_001::low_rank          0.4853945324
#> band_energy_y::thickness::myelin::none::component_001::low_rank          0.3160240738
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank  0.0012000430
#> spectral_coupling::thickness::myelin::none::component_001::low_rank      0.3355753196
#> retained_variance_x::thickness::myelin::none::component_001::low_rank    0.0062266333
#> retained_variance_y::thickness::myelin::none::component_001::low_rank    0.0068109948
#> band_energy_x::thickness::myelin::none::component_001::mid_rank          0.0091272839
#> band_energy_y::thickness::myelin::none::component_001::mid_rank          0.2234725002
#> spectral_cross_energy::thickness::myelin::none::component_001::mid_rank  0.2700646899
#> spectral_coupling::thickness::myelin::none::component_001::mid_rank      0.3147911604
#> retained_variance_x::thickness::myelin::none::component_001::mid_rank    0.0062266333
#> retained_variance_y::thickness::myelin::none::component_001::mid_rank    0.0068109948
#> band_energy_x::thickness::myelin::none::component_001::high_rank         0.0540587426
#> band_energy_y::thickness::myelin::none::component_001::high_rank         0.2974439449
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank 0.0849706410
#> spectral_coupling::thickness::myelin::none::component_001::high_rank     0.1304003728
#> retained_variance_x::thickness::myelin::none::component_001::high_rank   0.0062266333
#> retained_variance_y::thickness::myelin::none::component_001::high_rank   0.0068109948
#> absolute_energy::thickness::none::none::component_001::low_rank          0.4853945324
#> relative_energy::thickness::none::none::component_001::low_rank          0.3823453726
#> absolute_energy::thickness::none::none::component_001::mid_rank          0.0091272839
#> relative_energy::thickness::none::none::component_001::mid_rank          0.3110090780
#> absolute_energy::thickness::none::none::component_001::high_rank         0.0540587426
#> relative_energy::thickness::none::none::component_001::high_rank         0.0985276621
#> retained_variance::thickness::none::none::component_001::retained        0.0062266333
#> residual_energy::thickness::none::none::component_001::retained          0.0267022687
#> absolute_energy::myelin::none::none::component_001::low_rank             0.3160240738
#> relative_energy::myelin::none::none::component_001::low_rank             0.0213352844
#> absolute_energy::myelin::none::none::component_001::mid_rank             0.2234725002
#> relative_energy::myelin::none::none::component_001::mid_rank             0.0063455692
#> absolute_energy::myelin::none::none::component_001::high_rank            0.2974439449
#> relative_energy::myelin::none::none::component_001::high_rank            0.0038120512
#> retained_variance::myelin::none::none::component_001::retained           0.0068109948
#> residual_energy::myelin::none::none::component_001::retained             0.0045510680
#> band_energy_x::thickness::myelin::none::component_002::low_rank          0.3356391734
#> band_energy_y::thickness::myelin::none::component_002::low_rank          0.3095498507
#> spectral_cross_energy::thickness::myelin::none::component_002::low_rank  0.0815365376
#> spectral_coupling::thickness::myelin::none::component_002::low_rank      0.4901796951
#> retained_variance_x::thickness::myelin::none::component_002::low_rank    0.1549630719
#> retained_variance_y::thickness::myelin::none::component_002::low_rank    0.0103112208
#> band_energy_x::thickness::myelin::none::component_002::mid_rank          0.0073177801
#> band_energy_y::thickness::myelin::none::component_002::mid_rank          0.1790962712
#> spectral_cross_energy::thickness::myelin::none::component_002::mid_rank  0.0888282973
#> spectral_coupling::thickness::myelin::none::component_002::mid_rank      0.1109167398
#> retained_variance_x::thickness::myelin::none::component_002::mid_rank    0.1549630719
#> retained_variance_y::thickness::myelin::none::component_002::mid_rank    0.0103112208
#> band_energy_x::thickness::myelin::none::component_002::high_rank         0.0029797878
#> band_energy_y::thickness::myelin::none::component_002::high_rank         0.3209833894
#> spectral_cross_energy::thickness::myelin::none::component_002::high_rank 0.1653242337
#> spectral_coupling::thickness::myelin::none::component_002::high_rank     0.2242602288
#> retained_variance_x::thickness::myelin::none::component_002::high_rank   0.1549630719
#> retained_variance_y::thickness::myelin::none::component_002::high_rank   0.0103112208
#> absolute_energy::thickness::none::none::component_002::low_rank          0.3356391734
#> relative_energy::thickness::none::none::component_002::low_rank          0.2282556478
#> absolute_energy::thickness::none::none::component_002::mid_rank          0.0073177801
#> relative_energy::thickness::none::none::component_002::mid_rank          0.0788990622
#> absolute_energy::thickness::none::none::component_002::high_rank         0.0029797878
#> relative_energy::thickness::none::none::component_002::high_rank         0.1157074549
#> retained_variance::thickness::none::none::component_002::retained        0.1549630719
#> residual_energy::thickness::none::none::component_002::retained          0.1878433258
#> absolute_energy::myelin::none::none::component_002::low_rank             0.3095498507
#> relative_energy::myelin::none::none::component_002::low_rank             0.0147890307
#> absolute_energy::myelin::none::none::component_002::mid_rank             0.1790962712
#> relative_energy::myelin::none::none::component_002::mid_rank             0.0306656262
#> absolute_energy::myelin::none::none::component_002::high_rank            0.3209833894
#> relative_energy::myelin::none::none::component_002::high_rank            0.0004494123
#> retained_variance::myelin::none::none::component_002::retained           0.0103112208
#> residual_energy::myelin::none::none::component_002::retained             0.0079652248
#>                                                                          p_raw
#> same_location::thickness::myelin::none::whole_base::all                  0.005
#> band_energy_x::thickness::myelin::none::component_001::low_rank          0.005
#> band_energy_y::thickness::myelin::none::component_001::low_rank          0.005
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank  0.605
#> spectral_coupling::thickness::myelin::none::component_001::low_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_001::low_rank    0.310
#> retained_variance_y::thickness::myelin::none::component_001::low_rank    0.270
#> band_energy_x::thickness::myelin::none::component_001::mid_rank          0.165
#> band_energy_y::thickness::myelin::none::component_001::mid_rank          0.005
#> spectral_cross_energy::thickness::myelin::none::component_001::mid_rank  0.005
#> spectral_coupling::thickness::myelin::none::component_001::mid_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_001::mid_rank    0.310
#> retained_variance_y::thickness::myelin::none::component_001::mid_rank    0.270
#> band_energy_x::thickness::myelin::none::component_001::high_rank         0.005
#> band_energy_y::thickness::myelin::none::component_001::high_rank         0.005
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank 0.005
#> spectral_coupling::thickness::myelin::none::component_001::high_rank     0.005
#> retained_variance_x::thickness::myelin::none::component_001::high_rank   0.310
#> retained_variance_y::thickness::myelin::none::component_001::high_rank   0.270
#> absolute_energy::thickness::none::none::component_001::low_rank          0.005
#> relative_energy::thickness::none::none::component_001::low_rank          0.005
#> absolute_energy::thickness::none::none::component_001::mid_rank          0.165
#> relative_energy::thickness::none::none::component_001::mid_rank          0.005
#> absolute_energy::thickness::none::none::component_001::high_rank         0.005
#> relative_energy::thickness::none::none::component_001::high_rank         0.005
#> retained_variance::thickness::none::none::component_001::retained        0.310
#> residual_energy::thickness::none::none::component_001::retained          0.025
#> absolute_energy::myelin::none::none::component_001::low_rank             0.005
#> relative_energy::myelin::none::none::component_001::low_rank             0.045
#> absolute_energy::myelin::none::none::component_001::mid_rank             0.005
#> relative_energy::myelin::none::none::component_001::mid_rank             0.210
#> absolute_energy::myelin::none::none::component_001::high_rank            0.005
#> relative_energy::myelin::none::none::component_001::high_rank            0.360
#> retained_variance::myelin::none::none::component_001::retained           0.270
#> residual_energy::myelin::none::none::component_001::retained             0.365
#> band_energy_x::thickness::myelin::none::component_002::low_rank          0.005
#> band_energy_y::thickness::myelin::none::component_002::low_rank          0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::low_rank  0.005
#> spectral_coupling::thickness::myelin::none::component_002::low_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_002::low_rank    0.005
#> retained_variance_y::thickness::myelin::none::component_002::low_rank    0.110
#> band_energy_x::thickness::myelin::none::component_002::mid_rank          0.255
#> band_energy_y::thickness::myelin::none::component_002::mid_rank          0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::mid_rank  0.005
#> spectral_coupling::thickness::myelin::none::component_002::mid_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_002::mid_rank    0.005
#> retained_variance_y::thickness::myelin::none::component_002::mid_rank    0.110
#> band_energy_x::thickness::myelin::none::component_002::high_rank         0.440
#> band_energy_y::thickness::myelin::none::component_002::high_rank         0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::high_rank 0.005
#> spectral_coupling::thickness::myelin::none::component_002::high_rank     0.005
#> retained_variance_x::thickness::myelin::none::component_002::high_rank   0.005
#> retained_variance_y::thickness::myelin::none::component_002::high_rank   0.110
#> absolute_energy::thickness::none::none::component_002::low_rank          0.005
#> relative_energy::thickness::none::none::component_002::low_rank          0.005
#> absolute_energy::thickness::none::none::component_002::mid_rank          0.255
#> relative_energy::thickness::none::none::component_002::mid_rank          0.005
#> absolute_energy::thickness::none::none::component_002::high_rank         0.440
#> relative_energy::thickness::none::none::component_002::high_rank         0.005
#> retained_variance::thickness::none::none::component_002::retained        0.005
#> residual_energy::thickness::none::none::component_002::retained          0.005
#> absolute_energy::myelin::none::none::component_002::low_rank             0.005
#> relative_energy::myelin::none::none::component_002::low_rank             0.085
#> absolute_energy::myelin::none::none::component_002::mid_rank             0.005
#> relative_energy::myelin::none::none::component_002::mid_rank             0.015
#> absolute_energy::myelin::none::none::component_002::high_rank            0.005
#> relative_energy::myelin::none::none::component_002::high_rank            0.780
#> retained_variance::myelin::none::none::component_002::retained           0.110
#> residual_energy::myelin::none::none::component_002::retained             0.155
#>                                                                          p_maxT
#> same_location::thickness::myelin::none::whole_base::all                   0.005
#> band_energy_x::thickness::myelin::none::component_001::low_rank           0.005
#> band_energy_y::thickness::myelin::none::component_001::low_rank           0.005
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank   1.000
#> spectral_coupling::thickness::myelin::none::component_001::low_rank       0.005
#> retained_variance_x::thickness::myelin::none::component_001::low_rank     1.000
#> retained_variance_y::thickness::myelin::none::component_001::low_rank     1.000
#> band_energy_x::thickness::myelin::none::component_001::mid_rank           0.995
#> band_energy_y::thickness::myelin::none::component_001::mid_rank           0.005
#> spectral_cross_energy::thickness::myelin::none::component_001::mid_rank   0.005
#> spectral_coupling::thickness::myelin::none::component_001::mid_rank       0.005
#> retained_variance_x::thickness::myelin::none::component_001::mid_rank     1.000
#> retained_variance_y::thickness::myelin::none::component_001::mid_rank     1.000
#> band_energy_x::thickness::myelin::none::component_001::high_rank          0.020
#> band_energy_y::thickness::myelin::none::component_001::high_rank          0.005
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank  0.005
#> spectral_coupling::thickness::myelin::none::component_001::high_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_001::high_rank    1.000
#> retained_variance_y::thickness::myelin::none::component_001::high_rank    1.000
#> absolute_energy::thickness::none::none::component_001::low_rank           0.005
#> relative_energy::thickness::none::none::component_001::low_rank           0.005
#> absolute_energy::thickness::none::none::component_001::mid_rank           0.995
#> relative_energy::thickness::none::none::component_001::mid_rank           0.005
#> absolute_energy::thickness::none::none::component_001::high_rank          0.020
#> relative_energy::thickness::none::none::component_001::high_rank          0.005
#> retained_variance::thickness::none::none::component_001::retained         1.000
#> residual_energy::thickness::none::none::component_001::retained           0.465
#> absolute_energy::myelin::none::none::component_001::low_rank              0.005
#> relative_energy::myelin::none::none::component_001::low_rank              0.665
#> absolute_energy::myelin::none::none::component_001::mid_rank              0.005
#> relative_energy::myelin::none::none::component_001::mid_rank              1.000
#> absolute_energy::myelin::none::none::component_001::high_rank             0.005
#> relative_energy::myelin::none::none::component_001::high_rank             1.000
#> retained_variance::myelin::none::none::component_001::retained            1.000
#> residual_energy::myelin::none::none::component_001::retained              1.000
#> band_energy_x::thickness::myelin::none::component_002::low_rank           0.005
#> band_energy_y::thickness::myelin::none::component_002::low_rank           0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::low_rank   0.005
#> spectral_coupling::thickness::myelin::none::component_002::low_rank       0.005
#> retained_variance_x::thickness::myelin::none::component_002::low_rank     0.005
#> retained_variance_y::thickness::myelin::none::component_002::low_rank     0.995
#> band_energy_x::thickness::myelin::none::component_002::mid_rank           1.000
#> band_energy_y::thickness::myelin::none::component_002::mid_rank           0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::mid_rank   0.005
#> spectral_coupling::thickness::myelin::none::component_002::mid_rank       0.005
#> retained_variance_x::thickness::myelin::none::component_002::mid_rank     0.005
#> retained_variance_y::thickness::myelin::none::component_002::mid_rank     0.995
#> band_energy_x::thickness::myelin::none::component_002::high_rank          1.000
#> band_energy_y::thickness::myelin::none::component_002::high_rank          0.005
#> spectral_cross_energy::thickness::myelin::none::component_002::high_rank  0.005
#> spectral_coupling::thickness::myelin::none::component_002::high_rank      0.005
#> retained_variance_x::thickness::myelin::none::component_002::high_rank    0.005
#> retained_variance_y::thickness::myelin::none::component_002::high_rank    0.995
#> absolute_energy::thickness::none::none::component_002::low_rank           0.005
#> relative_energy::thickness::none::none::component_002::low_rank           0.005
#> absolute_energy::thickness::none::none::component_002::mid_rank           1.000
#> relative_energy::thickness::none::none::component_002::mid_rank           0.005
#> absolute_energy::thickness::none::none::component_002::high_rank          1.000
#> relative_energy::thickness::none::none::component_002::high_rank          0.005
#> retained_variance::thickness::none::none::component_002::retained         0.005
#> residual_energy::thickness::none::none::component_002::retained           0.005
#> absolute_energy::myelin::none::none::component_002::low_rank              0.005
#> relative_energy::myelin::none::none::component_002::low_rank              0.910
#> absolute_energy::myelin::none::none::component_002::mid_rank              0.005
#> relative_energy::myelin::none::none::component_002::mid_rank              0.325
#> absolute_energy::myelin::none::none::component_002::high_rank             0.005
#> relative_energy::myelin::none::none::component_002::high_rank             1.000
#> retained_variance::myelin::none::none::component_002::retained            0.995
#> residual_energy::myelin::none::none::component_002::retained              1.000
result$omnibus
#>   omnibus  statistic p_value
#> 1     max   24.06449   0.005
#> 2  sum_sq 3286.36286   0.005
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
