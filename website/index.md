---
title: neurogeo
description: 面向神经影像空间数据的 R 工具包
---

<img class="ng-home-logo" src="/logo.png" alt="neurogeo 标志">

# neurogeo

`neurogeo` 用统一的数据模型表示皮层表面、体素、CIFTI grayordinate、
脑区和点数据，并显式记录空间、测量语义、空间支持和处理历史。

> **6.0 审计状态：** 当前暂停新增公开 API，优先修复科学正确性、教程和
> 发布证据。surface spin 与 Moran eigen-sign surrogate 尚未完成 type-I
> 校准，只能在显式 opt-in 后用于方法评估，不能作为稳定推断。

## 我能用它做什么？

- 读取和写出 NIfTI、GIFTI、CIFTI 与 FreeSurfer 数据；
- 检查值、空间元素、layer 和 measure 是否严格对齐；
- 构造稀疏邻接、距离和空间权重；
- 在 vertex、voxel 和 parcel 支持之间进行可审计转换；
- 运行空间统计与模型，并保留 metric、support 和数值诊断；
- 为多被试和多 atlas 工作流建立可复现的分析对象。

本包不负责 MRI 预处理、配准、分割或表面重建，也不会在不兼容的空间间
自动重采样。

## 从哪里开始？

- [安装与第一个可运行示例](/guide/)
- [理解 base、layer、measure 和 support](/concepts/)
- [按数据格式和任务选择教程](/tutorials/)
- [功能模块](/modules/)
- [函数参考](/api/reference/)
- [English documentation](/en/)
