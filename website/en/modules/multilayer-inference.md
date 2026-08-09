---
title: "Multilayer spatial inference"
outline: [2, 3]
editLink: false
sourceSha256: "6809dcdc1587cad2c8b49d613a5dd12d80af651da9bbfaf5ab9c83224bb785e1"
---

**Edit source:** [Edit on GitHub](https://github.com/zh1peng/neurogeo/edit/main/vignettes/multilayer-inference.Rmd)


## Scientific question and data contract

This workflow asks whether two aligned cortical layers are coupled, at
which retained spatial scales that coupling occurs, and whether coupling
differs between subject groups. Every map belongs to one independent
subject and one layer. Vertices or regions are spatial elements, not
replicated subjects.

``` r
library(neurogeo)

set.seed(50)
n_element <- 10L
n_subject <- 16L
unit_id <- sprintf("sub-%02d", seq_len(n_subject))
group <- factor(rep(c("control", "case"), each = n_subject / 2L))
age <- seq(20, 55, length.out = n_subject)
feature <- rep(c("thickness", "myelin"), n_subject)
subject <- rep(unit_id, each = 2L)
base <- sin(seq_len(n_element) / 2)
values <- matrix(NA_real_, n_element, 2L * n_subject)
for (i in seq_len(n_subject)) {
  values[, 2L * i - 1L] <- base + rnorm(n_element, sd = 0.12)
  strength <- if (group[[i]] == "case") 0.8 else 0.4
  values[, 2L * i] <- strength * base + rnorm(n_element, sd = 0.12)
}
layers <- data.frame(
  layer_id = sprintf("map-%03d", seq_len(ncol(values))),
  name = paste(subject, feature, sep = "_"),
  subject_id = subject, feature = feature
)
measure <- ngeo_measure(support_behavior = "intensive", unit = "a.u.")
x <- ngeo_point(
  cbind(x = seq_len(n_element), y = 0), values = values, layers = layers,
  measures = measure[rep.int(1L, ncol(values)), , drop = FALSE]
)
index <- ngeo_validate_layers(
  x, required_layers = c("thickness", "myelin"), complete = "error"
)
```

## Fixed spatial estimand

The graph basis below depends on coordinates, topology, and declared
support, not on the values that will later be tested. The two named
bands are rank-matched retained modes; they are not automatically
physical wavelengths.

``` r
spatial_weights <- ngeo_spatial_weights(
  x, method = "distance_band", threshold = 1.01, style = "B"
)
basis <- ngeo_spatial_basis(
  x, spatial_weights, support = "identity", n_modes = n_element - 1L
)
features <- ngeo_layer_coupling(
  x, index, basis = basis,
  bands = list(low_rank = 1:4, high_rank = 5:9),
  estimands = c("same_location", "spectral_coupling", "band_energy")
)
features
#> <ngeo_subject_features>
#>   unit: 16
#>   endpoints: 25
#>   finite cells: 400/400
```

`spectral_coupling` is normalized cross-layer covariance in a band.
`band_energy_x` and `band_energy_y` are marginal layer structure.
Keeping them as separate endpoints prevents an energy change from being
described as a coupling change.

## Subject-level inference

The exchangeability object transforms whole subjects. One schedule is
reused for every endpoint; no vertex-wise pseudo-replication occurs.

``` r
design <- data.frame(unit_id = unit_id, group = group, age = age)
schedule <- ngeo_exchangeability(
  unit_id, scheme = "free", permutations = 99L, seed = 5001L
)
result <- ngeo_group_test(
  features, design, model = ~ age + group, test = "group",
  exchangeability = schedule, transform = "auto", adjustment = "maxT"
)
#> Warning: Large group-wise endpoint variance differences were detected;
#> permutation validity still assumes exchangeable residuals.
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
#> band_energy_x::thickness::myelin::none::component_001::high_rank                 band_energy_x
#> band_energy_y::thickness::myelin::none::component_001::high_rank                 band_energy_y
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank spectral_cross_energy
#> spectral_coupling::thickness::myelin::none::component_001::high_rank         spectral_coupling
#> retained_variance_x::thickness::myelin::none::component_001::high_rank     retained_variance_x
#> retained_variance_y::thickness::myelin::none::component_001::high_rank     retained_variance_y
#> absolute_energy::thickness::none::none::component_001::low_rank                absolute_energy
#> relative_energy::thickness::none::none::component_001::low_rank                relative_energy
#> absolute_energy::thickness::none::none::component_001::high_rank               absolute_energy
#> relative_energy::thickness::none::none::component_001::high_rank               relative_energy
#> retained_variance::thickness::none::none::component_001::retained            retained_variance
#> residual_energy::thickness::none::none::component_001::retained                residual_energy
#> absolute_energy::myelin::none::none::component_001::low_rank                   absolute_energy
#> relative_energy::myelin::none::none::component_001::low_rank                   relative_energy
#> absolute_energy::myelin::none::none::component_001::high_rank                  absolute_energy
#> relative_energy::myelin::none::none::component_001::high_rank                  relative_energy
#> retained_variance::myelin::none::none::component_001::retained               retained_variance
#> residual_energy::myelin::none::none::component_001::retained                   residual_energy
#>                                                                               band
#> same_location::thickness::myelin::none::whole_base::all                        all
#> band_energy_x::thickness::myelin::none::component_001::low_rank           low_rank
#> band_energy_y::thickness::myelin::none::component_001::low_rank           low_rank
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank   low_rank
#> spectral_coupling::thickness::myelin::none::component_001::low_rank       low_rank
#> retained_variance_x::thickness::myelin::none::component_001::low_rank     low_rank
#> retained_variance_y::thickness::myelin::none::component_001::low_rank     low_rank
#> band_energy_x::thickness::myelin::none::component_001::high_rank         high_rank
#> band_energy_y::thickness::myelin::none::component_001::high_rank         high_rank
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank high_rank
#> spectral_coupling::thickness::myelin::none::component_001::high_rank     high_rank
#> retained_variance_x::thickness::myelin::none::component_001::high_rank   high_rank
#> retained_variance_y::thickness::myelin::none::component_001::high_rank   high_rank
#> absolute_energy::thickness::none::none::component_001::low_rank           low_rank
#> relative_energy::thickness::none::none::component_001::low_rank           low_rank
#> absolute_energy::thickness::none::none::component_001::high_rank         high_rank
#> relative_energy::thickness::none::none::component_001::high_rank         high_rank
#> retained_variance::thickness::none::none::component_001::retained         retained
#> residual_energy::thickness::none::none::component_001::retained           retained
#> absolute_energy::myelin::none::none::component_001::low_rank              low_rank
#> relative_energy::myelin::none::none::component_001::low_rank              low_rank
#> absolute_energy::myelin::none::none::component_001::high_rank            high_rank
#> relative_energy::myelin::none::none::component_001::high_rank            high_rank
#> retained_variance::myelin::none::none::component_001::retained            retained
#> residual_energy::myelin::none::none::component_001::retained              retained
#>                                                                            coefficient
#> same_location::thickness::myelin::none::whole_base::all                  -9.255421e-01
#> band_energy_x::thickness::myelin::none::component_001::low_rank           4.514930e-01
#> band_energy_y::thickness::myelin::none::component_001::low_rank          -2.752106e+00
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank  -2.194227e+00
#> spectral_coupling::thickness::myelin::none::component_001::low_rank      -1.426938e+00
#> retained_variance_x::thickness::myelin::none::component_001::low_rank     1.149874e-16
#> retained_variance_y::thickness::myelin::none::component_001::low_rank     5.815454e-17
#> band_energy_x::thickness::myelin::none::component_001::high_rank          1.269574e-01
#> band_energy_y::thickness::myelin::none::component_001::high_rank         -1.828611e-02
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank  2.441561e-02
#> spectral_coupling::thickness::myelin::none::component_001::high_rank      3.554408e-01
#> retained_variance_x::thickness::myelin::none::component_001::high_rank    1.149874e-16
#> retained_variance_y::thickness::myelin::none::component_001::high_rank    5.815454e-17
#> absolute_energy::thickness::none::none::component_001::low_rank           4.514930e-01
#> relative_energy::thickness::none::none::component_001::low_rank          -2.123124e-02
#> absolute_energy::thickness::none::none::component_001::high_rank          1.269574e-01
#> relative_energy::thickness::none::none::component_001::high_rank          2.123124e-02
#> retained_variance::thickness::none::none::component_001::retained         1.149874e-16
#> residual_energy::thickness::none::none::component_001::retained          -8.458842e-17
#> absolute_energy::myelin::none::none::component_001::low_rank             -2.752106e+00
#> relative_energy::myelin::none::none::component_001::low_rank             -4.251857e-02
#> absolute_energy::myelin::none::none::component_001::high_rank            -1.828611e-02
#> relative_energy::myelin::none::none::component_001::high_rank             4.251857e-02
#> retained_variance::myelin::none::none::component_001::retained            5.815454e-17
#> residual_energy::myelin::none::none::component_001::retained             -5.458597e-16
#>                                                                            partial_r2
#> same_location::thickness::myelin::none::whole_base::all                  0.6541938012
#> band_energy_x::thickness::myelin::none::component_001::low_rank          0.0438401118
#> band_energy_y::thickness::myelin::none::component_001::low_rank          0.7896949765
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank  0.7546220272
#> spectral_coupling::thickness::myelin::none::component_001::low_rank      0.5962457804
#> retained_variance_x::thickness::myelin::none::component_001::low_rank    0.0235820840
#> retained_variance_y::thickness::myelin::none::component_001::low_rank    0.0074957701
#> band_energy_x::thickness::myelin::none::component_001::high_rank         0.4218374618
#> band_energy_y::thickness::myelin::none::component_001::high_rank         0.0162999532
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank 0.0407457753
#> spectral_coupling::thickness::myelin::none::component_001::high_rank     0.0469045968
#> retained_variance_x::thickness::myelin::none::component_001::high_rank   0.0235820840
#> retained_variance_y::thickness::myelin::none::component_001::high_rank   0.0074957701
#> absolute_energy::thickness::none::none::component_001::low_rank          0.0438401118
#> relative_energy::thickness::none::none::component_001::low_rank          0.4122482980
#> absolute_energy::thickness::none::none::component_001::high_rank         0.4218374618
#> relative_energy::thickness::none::none::component_001::high_rank         0.4122482980
#> retained_variance::thickness::none::none::component_001::retained        0.0235820840
#> residual_energy::thickness::none::none::component_001::retained          0.0005050744
#> absolute_energy::myelin::none::none::component_001::low_rank             0.7896949765
#> relative_energy::myelin::none::none::component_001::low_rank             0.2279056414
#> absolute_energy::myelin::none::none::component_001::high_rank            0.0162999532
#> relative_energy::myelin::none::none::component_001::high_rank            0.2279056414
#> retained_variance::myelin::none::none::component_001::retained           0.0074957701
#> residual_energy::myelin::none::none::component_001::retained             0.1398617855
#>                                                                          p_raw
#> same_location::thickness::myelin::none::whole_base::all                   0.01
#> band_energy_x::thickness::myelin::none::component_001::low_rank           0.59
#> band_energy_y::thickness::myelin::none::component_001::low_rank           0.01
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank   0.01
#> spectral_coupling::thickness::myelin::none::component_001::low_rank       0.01
#> retained_variance_x::thickness::myelin::none::component_001::low_rank     0.60
#> retained_variance_y::thickness::myelin::none::component_001::low_rank     0.75
#> band_energy_x::thickness::myelin::none::component_001::high_rank          0.01
#> band_energy_y::thickness::myelin::none::component_001::high_rank          0.70
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank  0.42
#> spectral_coupling::thickness::myelin::none::component_001::high_rank      0.45
#> retained_variance_x::thickness::myelin::none::component_001::high_rank    0.60
#> retained_variance_y::thickness::myelin::none::component_001::high_rank    0.75
#> absolute_energy::thickness::none::none::component_001::low_rank           0.59
#> relative_energy::thickness::none::none::component_001::low_rank           0.02
#> absolute_energy::thickness::none::none::component_001::high_rank          0.01
#> relative_energy::thickness::none::none::component_001::high_rank          0.02
#> retained_variance::thickness::none::none::component_001::retained         0.60
#> residual_energy::thickness::none::none::component_001::retained           0.94
#> absolute_energy::myelin::none::none::component_001::low_rank              0.01
#> relative_energy::myelin::none::none::component_001::low_rank              0.09
#> absolute_energy::myelin::none::none::component_001::high_rank             0.70
#> relative_energy::myelin::none::none::component_001::high_rank             0.09
#> retained_variance::myelin::none::none::component_001::retained            0.75
#> residual_energy::myelin::none::none::component_001::retained              0.17
#>                                                                          p_maxT
#> same_location::thickness::myelin::none::whole_base::all                    0.02
#> band_energy_x::thickness::myelin::none::component_001::low_rank            0.99
#> band_energy_y::thickness::myelin::none::component_001::low_rank            0.01
#> spectral_cross_energy::thickness::myelin::none::component_001::low_rank    0.01
#> spectral_coupling::thickness::myelin::none::component_001::low_rank        0.03
#> retained_variance_x::thickness::myelin::none::component_001::low_rank      1.00
#> retained_variance_y::thickness::myelin::none::component_001::low_rank      1.00
#> band_energy_x::thickness::myelin::none::component_001::high_rank           0.08
#> band_energy_y::thickness::myelin::none::component_001::high_rank           1.00
#> spectral_cross_energy::thickness::myelin::none::component_001::high_rank   1.00
#> spectral_coupling::thickness::myelin::none::component_001::high_rank       0.99
#> retained_variance_x::thickness::myelin::none::component_001::high_rank     1.00
#> retained_variance_y::thickness::myelin::none::component_001::high_rank     1.00
#> absolute_energy::thickness::none::none::component_001::low_rank            0.99
#> relative_energy::thickness::none::none::component_001::low_rank            0.09
#> absolute_energy::thickness::none::none::component_001::high_rank           0.08
#> relative_energy::thickness::none::none::component_001::high_rank           0.09
#> retained_variance::thickness::none::none::component_001::retained          1.00
#> residual_energy::thickness::none::none::component_001::retained            1.00
#> absolute_energy::myelin::none::none::component_001::low_rank               0.01
#> relative_energy::myelin::none::none::component_001::low_rank               0.53
#> absolute_energy::myelin::none::none::component_001::high_rank              1.00
#> relative_energy::myelin::none::none::component_001::high_rank              0.53
#> retained_variance::myelin::none::none::component_001::retained             1.00
#> residual_energy::myelin::none::none::component_001::retained               0.85
result$omnibus
#>   omnibus  statistic p_value
#> 1     max   6.986772    0.01
#> 2  sum_sq 232.453385    0.01
```

The valid inference unit is the independent subject. The result supports
a claim about the declared population only when residual exchangeability
and the subject design are defensible. It does not establish causality,
registration quality, equal physical scale across atlases, or
parcellation invariance.

## Declared support family

Construct features independently on every atlas or support, then pass
the named list to the same group call. This produces one common schedule
and one full-family max-T calculation. Support dispersion remains
descriptive.

``` r
family_result <- ngeo_group_test(
  features = list(DK68 = dk_features, Schaefer100 = schaefer_features),
  data = design, model = ~ age + group, test = "group",
  exchangeability = schedule
)
family_result$support$stability
```

Use `ngeo_qc()` on the inputs and inspect basis diagnostics, missing
endpoints, schedule hashes, and support scale labels before interpreting
the tests.
