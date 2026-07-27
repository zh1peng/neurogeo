---
title: 分析工作流
description: neurogeo 的可执行空间分析工作流
---

# 分析工作流

## 点数据与空间自相关

**入口：** [点数据与 Moran's I](/tutorials/getting-started)

输入为坐标矩阵和严格对齐的 values block。工作流包括：

- `ngeo_points()` 对象构造与严格验证；
- distance-band 空间权重；
- global Moran's I 与 permutation test；
- local Moran 与多重检验校正；
- 元素到区域的 change of support。

## 标准格式 I/O

**入口：** [格式 I/O 与验证](/tutorials/format-workflows)

使用包内固定校验和的格式样例验证：

- NIfTI array、voxel index 和 affine；
- GIFTI coordinates、faces 和 metric alignment；
- CIFTI brain models 与 grayordinate 顺序；
- FreeSurfer geometry、metric 和 transform metadata；
- write/read round-trip。
