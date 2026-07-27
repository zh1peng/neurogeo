---
title: neurogeo
description: Neuroimaging Geoinformatics Core Specification 的 R 参考实现
---

# neurogeo

`neurogeo` 是 Neuroimaging Geoinformatics Core Specification（NGCS）的 R
参考实现，用于表示神经影像空间数据、构造空间关系、执行空间统计并记录
provenance。

## 对象约束

```text
one spatial domain
+ one aligned values block
+ explicit space, topology, metric, measurement semantics, and provenance
```

支持 `surface`、`volume`、`points`、`grayordinates` 和 `regions` 五类基础
domain。domain 元素索引与 values 行严格对齐。

## 功能范围

- NIfTI、GIFTI、CIFTI 和 FreeSurfer 标准格式输入；
- surface、volume、points、grayordinates 和 regions 的几何诊断；
- 稀疏邻接、距离、空间权重和分区；
- Moran's I、LISA、variogram、GWR、kriging、SAR/SEM 和 CAR；
- change of support、atlas overlap 和支持不确定性；
- transform graph、manifest、artifact integrity 和 replay。

软件包不执行 raw MRI preprocessing、registration、segmentation 或 surface
reconstruction，也不会在不兼容空间之间隐式重采样。

## 文档

- [安装与基本用法](/guide/)
- [NGCS 数据模型](/concepts/)
- [分析工作流](/tutorials/)
- [功能模块](/modules/)
- [函数参考](/api/reference/)
