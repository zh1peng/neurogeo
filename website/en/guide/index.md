---
title: Installation and basic use
---

# Installation and basic use

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
x <- ngeo_points(
  coordinates,
  values = cbind(signal = c(1, 2, 3, 2, 4, 7, 3, 7, 9)),
  measures = ngeo_measure(spatial_semantics = "intensive")
)

ngeo_validate(x, "strict")
w <- ngeo_weights(
  x,
  method = "distance_band",
  threshold = 1.01,
  style = "W"
)
ngeo_moran(x, w, "signal", permutations = 999, seed = 2026)
```
