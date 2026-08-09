---
title: 神经影像用户术语表
description: neurogeo 6.0 中 base、layer、measure、metric、support 与 null 的用户定义
---

# 神经影像用户术语表

本页解释用户需要掌握的当前术语；历史名称只在 [6.0 migration](/api/articles/migration-6.0.html) 中出现。

| 术语 | 在 neurogeo 中的含义 | 常见误解 |
|---|---|---|
| spatial base | values 每一行所依附的有序空间元素及其几何/空间身份 | 不是任意“分析区域” |
| element | base 中一个有稳定顺序的 vertex、voxel、grayordinate、parcel 或 point | 不一定是体素 |
| values | 与 base 行严格对齐的数值块 | 不包含空间几何 |
| layer | values 的一列及其稳定 `layer_id` | 显示名称不保证唯一 |
| measure | 去重后的测量定义：unit、value type、support behavior、missing policy、aggregation | 不是随意的列注释 |
| coordinate space | 坐标参考、kind 和 unit；未知必须写成 `unknown` | 未声明不等于 MNI 或 mm |
| topology | 元素之间的连接结构，如 surface faces | 不等于带数值的 spatial weights |
| distance method | 为一次分析选择的距离定义 | surface 上的直线距离不等于 geodesic |
| spatial weights | 绑定到一个 base 的稀疏邻接/权重算子 | 不是 ensemble probability |
| support | 一个测量代表的空间范围或权重 | 不只是坐标点 |
| support map | target-by-source 的显式稀疏聚合/重采样算子 | 不执行配准或分割 |
| sampling unit | 在研究设计中独立抽样或可交换的单位 | atlas 或 voxel 不自动等于独立受试者 |
| null model | 生成比较分布的具体交换、旋转或模型假设 | “随机”不是完整定义 |
| estimand | 在已声明 base、metric、support 和设计下要估计的量 | 函数名称本身不足以定义 |
| uncertainty target | SE、区间或模拟分布实际描述的量 | 不一定包含所有数据/模型不确定性 |

对稳定科学结果使用 `ngeo_inference_contract()`，可一次查看最后六项解释信息。

**Language:** [English](/en/glossary/)
