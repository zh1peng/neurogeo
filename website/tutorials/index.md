---
title: 教程
description: 从研究问题开始的完整 neurogeo walkthrough
---

# 完整分析教程

这里的教程按照一次真实分析的顺序组织，而不是按照函数名称拆分。

## 第一次完整分析

### [从空间对象到 Moran's I](/tutorials/getting-started)

从一个 3 × 3 教学网格开始，依次完成对象构造、可视化检查、空间权重、全局 Moran's I、LISA、change of support 和结果审阅。

你会看到：

- 信号在空间中的分布；
- 距离权重形成的邻接网络；
- Moran 散点图；
- 局部聚集类型；
- 元素 support 到区域 support 的变化。

## 真实文件 walkthrough

### [NIfTI、GIFTI、CIFTI 与 FreeSurfer](/tutorials/format-workflows)

使用包内真实格式样例，展示文件读取、索引与 affine 检查、surface metric 可视化、grayordinate 顺序验证以及 round-trip。

::: warning 范围
neurogeo 不执行 raw MRI preprocessing、registration 或 segmentation。教程从已经生成的标准格式文件开始。
:::
