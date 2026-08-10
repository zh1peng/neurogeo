---
title: 教程路线
description: 按学习阶段和神经影像输入选择可执行教程
---

# 教程路线

主教程共享同一条神经影像案例主线：100 名 HC 与 100 名 SCZ 的 DK68 皮层厚度，
以及每组 5 名被试的 vertex-level sample data。空间关系、support、I/O、推断、
模型、执行与复现页面都会把结果放回脑图；小型点阵只保留在 API conformance
测试中，不再作为主教学叙事。

## 第一次使用

1. [安装与第一次运行](/guide/)
2. [15 分钟快速开始](/tutorials/getting-started)
3. [按 NIfTI、surface、CIFTI 或 ROI/cohort 选择工作流](/tutorials/format-workflows)

- [NIfTI volume：read → QC → analysis → plot → round-trip](/tutorials/workflow-volume)
- [GIFTI surface：geometry → topology → metric](/tutorials/workflow-surface)
- [CIFTI：brain models → components → analysis](/tutorials/workflow-cifti)
- [DK68 ROI × cohort：parcels 是 base，subjects 是 layers](/tutorials/workflow-roi-cohort)

## 理解对象

- [base、values、layer 与 measure](/modules/core-concepts)
- [用户术语表](/glossary/)
- [质量控制](/modules/quality-control)

## 构造空间关系

- [邻接、距离与空间权重](/modules/neighbors-and-weights)
- [分区与聚合](/modules/parcellation-and-aggregation)
- [空间 support 转换](/modules/change-of-support)
- [显式 transform 重采样](/modules/transform-aware-resampling)

## 分析与解释

- [Support uncertainty](/modules/support-uncertainty)
- [Support-aware inference](/modules/support-aware-inference)
- [200 名被试的多层组间推断](/modules/group-inference)
- [空间模型](/modules/spatial-modelling)
- [时空分析](/modules/spatiotemporal-analysis)

## 执行与复现

- [Vertex case-control 数据的 bounded execution](/modules/bounded-execution)
- [Scalable I/O](/modules/scalable-io)
- [DK cortical selection 的可复现 replay](/modules/reproducible-replay)

每篇稳定科学教程都应说明 estimand、sampling unit、null、metric、support 和 uncertainty target。实验方法单独标记，不与稳定入口混排。
