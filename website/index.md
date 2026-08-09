---
title: neurogeo
description: 面向神经影像空间数据与空间统计的可审计 R 工具包
---

<img class="ng-home-logo" src="/logo.png" alt="neurogeo 标志">

# 从神经影像文件到可解释的空间结果

`neurogeo` 把 vertex、voxel、grayordinate、parcel 或 point 上的数值与空间身份、测量语义、support 和处理历史放在同一个可验证工作流中。它适合已经完成预处理、配准或分割，接下来需要空间关系、聚合、统计建模和可复现输出的用户。

## 你手里的数据是什么？

- **NIfTI / volume：** 从 [affine、mask 和体素单位](/tutorials/format-workflows#nifti-volume) 开始。
- **GIFTI / FreeSurfer surface：** 从 [vertex 顺序、faces 和 coordinate role](/tutorials/format-workflows#gifti-或-freesurfer-surface) 开始。
- **CIFTI / grayordinate：** 从 [brain model、structure 和 component](/tutorials/format-workflows#cifti-grayordinate) 开始。
- **ROI × subject/cohort：** 从 [parcel base、subject layer 和 group design](/tutorials/format-workflows#roi-cohort) 开始。

上述入口均在两次点击内到达相应工作流。第一次使用请先完成 [15 分钟快速开始](/tutorials/getting-started)。

## 这个包会做什么？

- 读取和写出常见神经影像格式并核对 ordered elements；
- 显式构造 topology、distance、spatial weights 和 support operators；
- 区分 intensive、extensive、count 与 categorical 测量；
- 运行空间统计、support-aware 分析和受资源约束的模型；
- 记录 source identity、history、inference contract 与 portable manifest。

它不执行 MRI 预处理、配准、分割或表面重建，也不会在不兼容空间之间静默重采样。

## 选择文档类型

- [安装与第一次运行](/guide/)
- [教程路线](/tutorials/)
- [用户术语表](/glossary/)
- [方法、假设与限制](/modules/)
- [API reference](/api/reference/)
- [English documentation](/en/)

> **推断状态：** surface spin 和 Moran eigen-sign surrogate 仍是需要显式 opt-in 的未校准实验方法；在完成预注册 type-I/power 校准前，不应作为稳定 null inference。
