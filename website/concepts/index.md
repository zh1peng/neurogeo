---
title: neurogeo 5.1 数据模型
description: spatial base、values、layers、measures 与 history
---

# neurogeo 5.1 数据模型

> base 决定数据在哪里，layers 是放在这个空间基座上的数据图层，measures
> 描述这些数值测量的是什么。

## 五个核心组成部分

```text
ngeo
├── base
├── values
├── layers
├── measures
└── history
```

- `base`：所有 layer 共享的空间结构，包含稳定的 elements、geometry、
  coordinate space，以及可选 topology。
- `values`：`n_elements × n_layers` 的数值或分类数据。
- `layers`：每个 values 列的元数据，如 subject、session 与 `measure_id`。
- `measures`：测量定义，如 unit、value type、support behavior 与 aggregation。
- `history`：数据来源和已执行操作的自动记录。

100 名受试者的 DK68 cortical thickness 是 68×100 的 values、100 个 layers，
但通常只有 1 个 measure。

## 五种 spatial base

公开 base type 只有：`point`、`surface`、`volume`、`parcellation` 和
`grayordinate`。各类型的坐标、网格、仿射、parcel membership 或 brain-model
mapping 都放在 `base$geometry` 中。

## 按需引入空间复杂度

- `topology` 回答哪些 element 在结构上相邻；
- `distance_method` 定义距离如何计算；
- `spatial_weights` 是一次分析实际使用的关系矩阵；
- `support` 是一个数值代表的空间范围；
- `transform` 映射不同 coordinate spaces。

它们不是所有数据对象的必填顶层字段。绘图需要 geometry，测地分析需要 surface
geometry 与 topology，空间统计需要 spatial weights，`aggregate_to()` 需要 support
和 measure aggregation semantics，跨空间映射则需要 coordinate space 与 transform。

## 空间聚合

```r
atlas <- ngeo_atlas_map(surface, atlas_labels)
regional <- aggregate_to(surface, atlas$target, atlas)
```

这个操作在统计意义上执行 change of spatial support，但用户 API 使用更直观的
`aggregate_to()`。
