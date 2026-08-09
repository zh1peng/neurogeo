---
title: 教程路线
description: 按数据格式和研究任务选择教程
---

# 教程路线

第一次使用时，建议先完成[安装与第一个空间分析](/guide/)，再按自己的输入
数据进入对应路线。

## 按数据格式

- **NIfTI / volume**：读取体素网格、检查 affine 与单位、处理 mask，再建立
  voxel 邻接或 world-space 距离；
- **GIFTI / FreeSurfer surface**：区分 anatomical、registration 和
  visualization coordinates，检查 faces 与 component，再使用 geodesic 距离；
- **CIFTI / grayordinate**：先检查 brain model、structure 和 surface/volume
  component，禁止跨结构的隐式距离；
- **ROI / cohort matrix**：一个稳定 parcel base 对应多名被试的 layers，
  `measure_id` 连接去重后的测量定义。

当前可运行的格式示例见[格式工作流](/tutorials/format-workflows)，完整的
15 分钟双语 quickstart 与许可固定的教学数据正在按 6.0 审计计划重建。

## 按任务

1. [读取真实数据](/modules/reading-data)
2. [核心对象与语义](/modules/core-concepts)
3. [邻居、距离与空间权重](/modules/neighbors-and-weights)
4. [分区与聚合](/modules/parcellation-and-aggregation)
5. [support 转换](/modules/change-of-support)
6. [空间模型](/modules/spatial-modelling)

## 推断安全提示

surface spin 和 Moran eigen-sign surrogate 目前是
`experimental_uncalibrated`。它们要求显式 opt-in，仅用于方法评估；在完成
预注册 type-I/power 校准前，不应把输出解释为稳定 p 值或 null inference。
