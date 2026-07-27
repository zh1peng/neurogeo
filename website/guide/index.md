---
title: 安装与基本用法
description: neurogeo 的安装、对象构造、验证和基本空间统计
---

# 安装与基本用法

## 系统要求

- R 4.2.0 或更高版本；
- 核心依赖：`Matrix`、`digest`；
- 格式后端按需安装：`RNifti`、`gifti`、`cifti`、
  `freesurferformats`。

FreeSurfer、FSL 和 Connectome Workbench 不是运行时依赖。

## 安装

```r
install.packages("remotes")
remotes::install_github("zh1peng/neurogeo")
```

## 构造空间对象

```r
library(neurogeo)

coordinates <- as.matrix(expand.grid(x = 0:2, y = 0:2))
signal <- c(1, 2, 3, 2, 4, 7, 3, 7, 9)

x <- ngeo_points(
  coordinates = coordinates,
  values = cbind(signal = signal),
  measures = ngeo_measure(
    value_type = "continuous",
    spatial_semantics = "intensive",
    units = "a.u."
  ),
  space = ngeo_space(
    space_id = "example-grid",
    kind = "unknown",
    units = "mm"
  )
)
```

## 验证

```r
ngeo_validate(x, level = "strict")
ngeo_domain_type(x)
ngeo_values(x)
ngeo_measures(x)
```

## 空间权重与 Moran's I

```r
w <- ngeo_weights(
  x,
  method = "distance_band",
  threshold = 1.01,
  metric = "euclidean",
  style = "W"
)

result <- ngeo_moran(
  x,
  weights = w,
  map = "signal",
  permutations = 999,
  seed = 2026
)

plot(x, map = "signal")
plot(result)
```

完整计算过程见[点数据与 Moran's I](/tutorials/getting-started)。
