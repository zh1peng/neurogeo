---
title: 安装与基本用法
description: 使用 spatial base、layers 与 measures 构造 neurogeo 对象
---

# 安装与基本用法

`neurogeo` 把神经影像数据表示为定义在同一空间基座上的一个或多个数据图层。

```r
install.packages("remotes")
remotes::install_github("zh1peng/neurogeo")
```

## 第一个对象

```r
library(neurogeo)

coordinates <- as.matrix(expand.grid(x = 0:2, y = 0:2))
x <- ngeo_point(
  coordinates,
  values = cbind(signal = c(1, 2, 3, 2, 4, 7, 3, 7, 9)),
  measures = ngeo_measure(
    support_behavior = "intensive",
    unit = "a.u."
  ),
  coordinate_space = ngeo_coordinate_space(
    "example-grid",
    unit = "mm"
  )
)
```

这个对象只有一个空间基座、一个 values 矩阵、一个 layer 和一个 measure。

```r
spatial_base(x)
values(x)
layers(x)
measures(x)
history(x)
```

始终满足两个对齐规则：`values[i, ]` 对应第 `i` 个 base element；
`values[, j]` 对应 `layers(x)[j, ]`。

## 空间分析

空间权重是针对一次分析构造的对象，不是 base 的固有字段：

```r
w <- ngeo_spatial_weights(
  x,
  method = "distance_band",
  threshold = 1.01,
  distance_method = "euclidean",
  style = "W"
)

ngeo_moran(x, w, map = "signal", permutations = 999, seed = 2026)
```

构造器只验证数据对齐；具体操作会在需要时检查 geometry、topology、support、
coordinate space 或 transform 等能力。
