---
title: 功能模块
description: neurogeo 的对象、I/O、空间关系、模型、执行和复现模块
---

# 功能模块

站点收录 2 篇中文分析工作流和 20 篇中文模块文档。每篇模块文档均提供对应
英文版本，并在页面顶部提供语言入口。

## 中文工作流

- [点数据空间统计：从 ngeo_points 到 Moran's I](/tutorials/getting-started)
- [标准格式 I/O：NIfTI、GIFTI、CIFTI 与 FreeSurfer](/tutorials/format-workflows)

## 数据模型与 I/O

- [核心概念与对象契约](/modules/core-concepts)
- [读取神经影像数据](/modules/reading-data)
- [Schema 验证与可移植 manifest](/modules/schema-validation)
- [皮层二维地图：从 vertex 数据到任意 atlas](/modules/cortical-cartography)
- [互操作与可审计交换](/modules/interoperability)
- [可扩展 values、CIFTI 与 BIDS derivatives](/modules/scalable-io)
- [文件后端神经影像 values](/modules/file-backed-io)

## 空间关系与 support

- [邻接关系与空间权重](/modules/neighbors-and-weights)
- [分区与聚合](/modules/parcellation-and-aggregation)
- [空间支持变换与跨 atlas 分析](/modules/change-of-support)
- [真实数据中的 support mapping](/modules/real-world-support-mapping)
- [显式 transform 的 resampling](/modules/transform-aware-resampling)
- [显式空间与 transform path](/modules/space-transform-graph)

## 推断与模型

- [Support uncertainty 与 operator ensemble](/modules/support-uncertainty)
- [Support-aware inference](/modules/support-aware-inference)
- [有界空间建模](/modules/spatial-modelling)
- [包含不确定性的空间模型](/modules/model-uncertainty)
- [有界迭代空间模型](/modules/iterative-spatial-models)
- [显式时间与时空分析](/modules/spatiotemporal-analysis)

## 执行与复现

- [有界科学计算](/modules/bounded-execution)
- [可审计 replay 与 derivative artifact](/modules/reproducible-replay)
