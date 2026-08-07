---
title: 教程路线
description: 从 base 与 layers 逐步进入空间关系、support 和 coordinate spaces
---

# 教程路线

建议按以下顺序学习：

1. **Base and layers**：只学习 `spatial_base()`、`values()`、`layers()` 与
   `measures()`，并理解行列对齐不变量。
2. **Spatial relationships**：学习 topology、distance methods 和
   `ngeo_spatial_weights()`。
3. **Spatial support and aggregation**：学习 support map、measure aggregation
   semantics 与 `aggregate_to()`。
4. **Coordinate spaces**：学习 `ngeo_coordinate_space()`、transform graph 和
   明确授权的跨空间映射。
5. **Reproducibility**：最后查看 `history()`、checksums 和内部 provenance 记录。

## DK68 心智模型

对于 100 名受试者的 DK68 cortical thickness：

- base 是 68 个有稳定顺序的 cortical parcels；
- values 是 68×100；
- layers 有 100 行，保存 subject、session 和 `measure_id`；
- measures 只有一行，定义 cortical thickness、mm、intensive 和
  area-weighted mean；
- parcel support、邻接、测地距离和空间权重只在相关分析需要时出现。

所有教程和 API 都使用 5.1 的 `base + layers` 模型，不提供 5.0 术语兼容层。
