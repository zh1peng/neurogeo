---
title: neurogeo 6.0 核心概念
description: base、layer、measure、support、space 与 metric
---

# neurogeo 6.0 核心概念

## 对象的五个组成部分

```text
ngeo
|-- base
|-- values
|-- layers
|-- measures
`-- history
```

- `base` 保存有稳定顺序的空间元素和几何信息；
- `values` 是 `n_elements × n_layers` 的数值或分类数据；
- `layers` 保存每列数据的被试、session、condition 和 `measure_id`；
- `measures` 定义单位、数值类型、support behavior 和聚合规则；
- `history` 记录数据来源和已经执行的操作。

例如，100 名被试的 DK68 cortical thickness 通常是一个 68 × 100 的
`values` 矩阵、100 个 layer，以及一个表示 cortical thickness 的 measure。

## 五类 spatial base

`point`、`surface`、`volume`、`parcellation` 和 `grayordinate` 是五类 base。
坐标、mesh faces、voxel affine、parcel membership 或 CIFTI brain models
属于 base geometry，不属于 layer。

## 容易混淆的空间概念

- **coordinate space** 说明坐标属于哪个模板或参考空间；
- **coordinate set** 是 surface 上的一组具体坐标，例如 anatomical、sphere
  或 visualization coordinates；
- **distance method / metric** 说明距离怎样计算，例如 Euclidean、
  edge-geodesic 或 hops；
- **spatial weights** 是一次分析实际使用的邻接或权重矩阵；
- **support** 是一个数值所代表的面积、体积、vertex 集合或 parcel 范围；
- **transform** 描述两个 coordinate spaces 之间的已知映射。

registration、visualization 和 chart 坐标不能被当作 anatomical metric。
surface 上的非邻接权重默认使用 edge-geodesic，避免跨过不连通结构或折叠。

## layer name 与 layer ID

用户可以给 layer 一个便于阅读的 name，但稳定选择应优先使用唯一
`layer_id`。若多个 layer 共用同一 name，`neurogeo` 会报歧义并列出可用 ID，
不会静默选择第一个。

## spatial support 为什么重要？

vertex、voxel 和 parcel 的数值代表不同空间范围。intensive measure（如平均
厚度）转换 support 时需要按面积或体积加权；extensive measure（如总面积）
需要守恒求和。只改变标签而不声明 support operator 会改变统计问题。
