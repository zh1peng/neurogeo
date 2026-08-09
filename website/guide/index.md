---
title: 安装与第一个空间分析
description: 用可直接运行的示例认识 neurogeo 6.0
---

# 安装与第一个空间分析

## 先理解三个词

- **spatial base**：数值位于哪里，例如 vertex、voxel 或 parcel；
- **layer**：同一个 base 上的一列观测，例如一名被试的 cortical thickness；
- **measure**：这列数值表示什么，包括单位、数值类型和聚合规则。

因此，`values(x)[i, j]` 总是对应第 `i` 个空间元素和第 `j` 个 layer。

## 安装

需要 R 4.2 或更新版本。仓库目前没有 6.0 release tag；`main` 是明确标注的
开发评估版本，不是稳定发布：

```r
install.packages("remotes")
remotes::install_github("zh1peng/neurogeo@main")
```

本地源码安装命令、三平台差异、版本固定规则及每种格式/方法的替代路径见
[安装与可选后端](/guide/installation)。

## 一个完整的小例子

下面构造 3 × 3 点网格、检查对象、建立空间权重，并计算 Moran's I。

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
    space_id = "example-grid",
    kind = "unknown",
    unit = "mm"
  )
)

ngeo_validate(x, "strict")
ngeo_spatial_base(x)
ngeo_layers(x)
ngeo_measures(x)

w <- ngeo_spatial_weights(
  x,
  method = "distance_band",
  threshold = 1.01,
  distance_method = "euclidean",
  style = "W"
)

ngeo_moran(
  x,
  w,
  layer = "signal",
  permutations = 999,
  seed = 2026
)
```

这里的 sampling unit 是网格点，邻居由 1.01 mm 的欧氏距离带定义。
Moran's I 描述这张 layer 在该权重定义下的空间自相关，不表示因果关系。

## 下一步

先阅读[核心概念](/concepts/)，再按自己的数据格式进入[教程路线](/tutorials/)。
