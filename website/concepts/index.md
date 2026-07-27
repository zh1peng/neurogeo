---
title: NGCS 数据模型
description: domain、support、space、topology、metric 与 measurement semantics
---

# NGCS 数据模型

每个 `ngeo` 对象包含一个 spatial domain 和一个严格对齐的 values block。
space、topology、metric、measurement semantics 和 provenance 均为显式字段。

## 核心字段

- **domain**：values 所依附的顶点、体素、点、grayordinates 或 regions；
- **element indexing**：domain 元素与 values 行之间的稳定一一对应；
- **support**：每个测量值代表的空间范围或数量基础；
- **space**：坐标参考、单位、结构和模板信息；
- **topology**：元素之间的结构连接；
- **metric**：距离或路径长度的定义；
- **measurement semantics**：support 改变时的聚合规则；
- **provenance**：输入、参数和操作记录。

## 语义区分

### topology 不等于 metric

拓扑回答“谁和谁连接”，metric 回答“连接或路径有多长”。同一张表面网格可以同时有 mesh adjacency、edge-geodesic distance 和其他经过声明的距离。

### geometry 不等于 support

几何描述位置和形状；support 描述一个数值代表的空间范围或数量基础。一个顶点坐标本身不能告诉你厚度值应如何聚合。

### 数值类型不等于测量语义

浮点数既可能是需要加权平均的厚度，也可能是需要守恒求和的总量。软件无法只看数值判断其科学含义，因此 measurement semantics 必须由研究者声明。
