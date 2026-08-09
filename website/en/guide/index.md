---
title: Installation and basic use
---

# Installation and basic use

Surface spin and the Moran eigen-sign surrogate currently require explicit
experimental opt-in and must not be interpreted as calibrated null inference.

## Requirements

- R 4.2.0 or later;
- `RNifti`, `gifti`, `cifti`, and `freesurferformats` for the corresponding
  optional file formats.

## Installation

```r
install.packages("remotes")
remotes::install_github("zh1peng/neurogeo")
```

## Basic spatial object

```r
library(neurogeo)

coordinates <- as.matrix(expand.grid(x = 0:2, y = 0:2))
x <- ngeo_point(
  coordinates,
  values = cbind(signal = c(1, 2, 3, 2, 4, 7, 3, 7, 9)),
  measures = ngeo_measure(support_behavior = "intensive")
)

spatial_base(x)
layers(x)
measures(x)
ngeo_validate(x, "strict")
w <- ngeo_spatial_weights(
  x,
  method = "distance_band",
  threshold = 1.01,
  style = "W"
)
ngeo_moran(x, w, layer = "signal", permutations = 999, seed = 2026)
```

The values rows align with base elements and the values columns align with
layer rows. Measures are referenced from layers through `measure_id`.
