---
title: 学习路线
description: 从第一次运行到能够独立审阅 neurogeo 分析
---

# 学习路线

不要从函数列表开始学习 neurogeo。更有效的方法是先完成一个小而完整的空间分析，再把相同思路迁移到真实神经影像格式。

<div class="learning-path">
  <div><strong>第一步</strong>创建空间对象，并理解 domain 与 values 为什么必须严格对齐。</div>
  <div><strong>第二步</strong>把信号和邻接关系画出来，在计算前发现空间错误。</div>
  <div><strong>第三步</strong>计算 Moran's I 与 LISA，理解公式、置换分布和空间聚类。</div>
  <div><strong>第四步</strong>读取 NIfTI、GIFTI、CIFTI 和 FreeSurfer 数据并验证。</div>
</div>

## 建议顺序

1. [第一次完整分析](/tutorials/getting-started)：适合第一次接触空间统计和 neurogeo。
2. [真实格式读取与验证](/tutorials/format-workflows)：把同样的检查顺序应用到真实神经影像文件。
3. [NGCS 概念地图](/concepts/)：在完成实践后系统整理 domain、support、space 和 measurement semantics。
4. [函数参考](/api/reference/)：知道分析目标后再查询参数和返回值。

::: tip 阅读方式
先观察每幅图并写下预期，再运行对应代码。空间分析中，图形不是最后的装饰，而是检查数据与模型假设的组成部分。
:::
