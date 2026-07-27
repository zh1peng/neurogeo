---
title: NGCS 概念地图
description: domain、support、space、topology、metric 与 measurement semantics
---

# NGCS 概念地图

neurogeo 的对象约束可以压缩成一句话：

> 一个空间 domain，对应一个严格对齐的 values block；空间、拓扑、度量、测量语义和 provenance 都必须显式声明。

## 数据对象的组成

<div class="learning-path">
  <div><strong>domain</strong>值依附在哪些顶点、体素、点、grayordinates 或 regions 上。</div>
  <div><strong>values</strong>第 i 行始终对应 domain 的第 i 个元素。</div>
  <div><strong>space + metric</strong>坐标属于哪个空间，以及距离应如何定义。</div>
  <div><strong>semantics</strong>support 改变时应求均值、守恒求和还是使用众数。</div>
</div>

## 三个不能混淆的问题

### topology 不等于 metric

拓扑回答“谁和谁连接”，metric 回答“连接或路径有多长”。同一张表面网格可以同时有 mesh adjacency、edge-geodesic distance 和其他经过声明的距离。

### geometry 不等于 support

几何描述位置和形状；support 描述一个数值代表的空间范围或数量基础。一个顶点坐标本身不能告诉你厚度值应如何聚合。

### 数值类型不等于测量语义

浮点数既可能是需要加权平均的厚度，也可能是需要守恒求和的总量。软件无法只看数值判断其科学含义，因此 measurement semantics 必须由研究者声明。

下一步请在[第一次完整分析](/tutorials/getting-started)中观察这些概念如何进入实际计算。
