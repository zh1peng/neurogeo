# neurogeo Core Data Model Refactor

## 1. Refactor goal

当前 neurogeo 的底层设计试图显式描述空间分析所需的多个组成部分，包括 domain、values、maps、measures、support、space、topology、metric、weights 和 provenance。

这些概念本身大多有明确的空间统计学意义，但目前存在两个问题：

1. 不同抽象层级的概念被并列呈现；
2. `domain`、`maps`、`provenance` 等名称对于神经影像用户不够直观。

本次 refactor 的目标不是简化空间统计本身，而是建立一个更容易理解的用户心智模型：

> **A neurogeo object consists of a spatial base and one or more aligned data layers.**

即：

> **一个 neurogeo 对象由一个空间基座，以及定义在该基座上的一个或多个数据图层组成。**

核心模型应尽可能接近 raster / multilayer spatial data 的直觉，同时能够统一：

- voxel volumes；
- cortical surfaces；
- parcellations；
- CIFTI grayordinates；
- arbitrary coordinates；
- multilayer brain maps。

---

# 2. New conceptual model

核心对象简化为五个主要组成部分：

```text
neurogeo object
│
├── base
├── values
├── layers
├── measures
└── history
```

空间分析需要的其他结构：

```text
support
topology
coordinate_space
distance
spatial_weights
transform
```

不再与核心数据组件处于同一概念层级。

---

# 3. Terminology refactor

| Current      | Proposed                       | Role                       |
| ------------ | ------------------------------ | -------------------------- |
| `domain`     | `base` / spatial base          | 数据所依附的空间基座                 |
| `values`     | `values`                       | 实际数值                       |
| `maps`       | `layers`                       | values 中每一列所对应的数据图层        |
| `measures`   | `measures`                     | 数值所代表的测量类型和语义              |
| `support`    | `support`                      | 每个空间元素所代表的空间范围             |
| `provenance` | `history`                      | 数据来源和操作历史                  |
| `space`      | `coordinate_space`             | 几何坐标所属参考空间                 |
| `topology`   | `topology`                     | 空间元素之间的结构连接                |
| `metric`     | `distance` / `distance_method` | 空间距离的定义方法                  |
| `weights`    | `spatial_weights`              | 某个分析实际使用的空间权重              |
| `transform`  | `transform`                    | 不同 coordinate spaces 之间的映射 |

`measures` 保留现有名称，不改为 variables。

---

# 4. The spatial base

## 4.1 Definition

`base` 是整个 neurogeo 数据对象的空间基座：

> the shared spatial structure on which data layers are defined.

它回答：

> **这些数值放在哪里？**

例如 DK68：

```text
base
    Desikan–Killiany parcellation
    68 cortical parcels
```

surface：

```text
base
    fsaverage cortical mesh
    163842 vertices / hemisphere
```

NIfTI：

```text
base
    MNI152 voxel grid
```

CIFTI：

```text
base
    grayordinate structure
```

coordinates：

```text
base
    arbitrary spatial points
```

---

## 4.2 Base types

建议使用有限的 base 类型：

```text
point
surface
volume
parcellation
grayordinate
```

避免继续增加大量特殊 object types。

---

## 4.3 What belongs to the base

一个 base 至少描述：

```text
base
├── type
├── elements
├── geometry
├── coordinate_space
└── topology (optional)
```

### `elements`

定义 values 行对应的稳定元素及顺序。

DK68：

```text
1   lh_bankssts
2   lh_caudalanteriorcingulate
...
68  rh_insula
```

### `geometry`

描述这些元素实际在哪里。

不同 base 类型具有不同 geometry：

```text
point         → coordinates
surface       → vertices + faces
volume        → voxel grid + affine
parcellation  → parcel membership / geometry
grayordinate  → brain-model mapping
```

### `coordinate_space`

描述 geometry 所在参考空间：

```text
fsaverage
fsLR-32k
MNI152
subject-native
```

### `topology`

仅当 base 自身具有内在拓扑时保存。

例如：

```text
surface:
    mesh edges

volume:
    voxel adjacency may be derived

parcellation:
    parcel adjacency may be derived from boundaries
```

因此 topology 可以存在于 base 中，但不是所有 base 强制要求的字段。

---

# 5. Values

`values` 仍然保持最简单的定义：

> numeric or categorical values aligned to the base elements.

基本形状：

```text
n_elements × n_layers
```

例如 100 名受试者的 DK68 cortical thickness：

```text
68 × 100
```

其中：

```text
rows    → DK68 parcels
columns → data layers
```

最重要的不变量是：

> `values[i, ]` 永远对应 `base$elements[i]`。

这个 alignment 是对象 invariant，而不是独立的数据字段。

---

# 6. Layers

将 `maps` 改为 `layers`。

这是本次 refactor 中除 `domain → base` 外最重要的概念变化。

## 6.1 Definition

一个 layer 是：

> one complete set of values defined over the shared spatial base.

例如：

```text
DK68 base
│
├── layer 1: subject 001 cortical thickness
├── layer 2: subject 002 cortical thickness
├── layer 3: subject 003 cortical thickness
└── ...
```

每个 layer 对应 `values` 的一列。

---

## 6.2 Layer metadata

`layers` 本身保存 metadata，而不是重复保存数值：

```text
layer_id
measure_id
subject_id
session
group
source
...
```

例如：

| layer\_id | subject | session  | measure\_id |
| --------- | ------- | -------- | ----------- |
| L001      | sub-001 | baseline | thickness   |
| L002      | sub-002 | baseline | thickness   |
| L003      | sub-003 | followup | thickness   |

因此：

```text
values[, j]
```

对应：

```text
layers[j, ]
```

---

# 7. Measures

`measures` 保留。

这是一个重要且合理的概念，不建议重命名。

## 7.1 Definition

Measure 回答：

> **这一层中的数字究竟测量了什么？**

例如 cortical thickness：

```text
measure_id        thickness
name              cortical thickness
unit              mm
value_type        continuous
support_behavior  intensive
aggregation       area_weighted_mean
```

surface area：

```text
measure_id        surface_area
unit              mm²
support_behavior  extensive
aggregation       sum
```

---

## 7.2 Layers vs measures

必须在教程中明确解释两者。

100 个受试者都有 cortical thickness：

```text
100 layers
1 measure
```

如果 100 个受试者同时有 cortical thickness 和 surface area：

```text
200 layers
2 measures
```

其关系为：

```text
layer
    ↓ refers to
measure
```

因此：

```text
layers$measure_id
```

用于连接 layers 和 measures。

---

# 8. Support

`support` 保留空间统计中的正式术语，但不再作为入门教程的核心字段。

## 8.1 Definition

Support 表示：

> **the spatial footprint represented by one value.**

即：

> 一个数值实际代表多大的空间范围。

例如 DK68：

```text
base element:
    lh_insula

support:
    DK68 lh_insula 所覆盖的 cortical surface
```

因此：

```text
parcellation
    = 整套空间划分方案

base elements
    = 68 个 parcels

support
    = 每个 parcel 实际覆盖的空间区域
```

Parcellation 和 support 高度相关，但不是同义词。

---

## 8.2 Support should often be implicit

不要求所有对象显式提供 support。

### voxel

support 可由 voxel geometry 自动获得。

### parcel

support 可由 atlas/parcellation geometry 自动获得。

### surface vertex

support 可以通过 vertex-associated area / dual-cell area 推导。

### point

support 可以是未知：

```text
support = NULL
```

这完全合法。

---

## 8.3 Support only becomes important when needed

普通空间分析：

```text
plot
Moran's I
local statistics
distance calculation
```

通常不需要用户显式理解 support。

以下操作才真正依赖 support：

```text
vertex → parcel
voxel → ROI
DK68 → lobe
fine atlas → coarse atlas
cross-resolution aggregation
```

---

# 9. Change of support

`change of support` 保留为空间统计学正式术语，但不应成为用户首先遇到的 API 名称。

定义：

> transforming a spatial layer from one spatial support/base to another.

例如：

```text
vertex surface
      ↓
DK68
```

或者：

```text
DK68
      ↓
lobes
```

本质上是：

```text
source base
    +
source values
    +
source/target spatial overlap
    +
measure aggregation semantics
         ↓
target base + target values
```

因此建议用户 API 使用更直观的名称，例如：

```text
aggregate_to()
```

或：

```text
rebase()
```

文档中说明：

> This operation performs a change of spatial support.

建议优先使用：

```text
aggregate_to()
```

因为 `rebase()` 对 DK68 → Schaefer400 这种非严格 aggregation 情况可能语义过宽，而 `change_support()` 又过于专业。

---

# 10. History

将 `provenance` 用户层名称改为：

```text
history
```

定义：

> where the object came from and what operations have been applied to it.

例如：

```text
read FreeSurfer aparc.stats
→ combine hemispheres
→ align DK68 ordering
→ attach fsaverage geometry
→ aggregate layers
```

History 应尽可能：

- 自动记录；
- 默认不干扰分析；
- 可以随时查询；
- 用于 reproducibility 和 debugging。

用户不应该手动构造复杂 provenance。

内部实现仍可以使用 provenance vocabulary，例如：

```text
provenance_record
provenance_hash
```

但 public API 应以：

```text
history(x)
```

为主。

---

# 11. Spatial relationships

以下概念不属于核心数据本身，而属于 base 上定义的空间关系或分析对象。

## Topology

回答：

> 哪些元素在结构上相邻？

例如 DK68：

```text
parcel A shares cortical boundary with parcel B
```

surface：

```text
vertices connected by mesh edges
```

保留：

```text
topology
```

不建议改成 connectivity，因为在神经影像中 connectivity 会与 FC/SC 混淆。

---

## Distance

将 `metric` 用户层概念改为：

```text
distance
```

或：

```text
distance_method
```

例如：

```text
euclidean
geodesic
graph
centroid
```

推荐：

```text
distance_method
```

对象则可以叫：

```text
ngeo_distance
```

---

## Spatial weights

将 `weights` 明确称为：

```text
spatial_weights
```

因为 weights 是：

> 针对某次空间分析构造出来的关系矩阵。

同一个 DK68 base 可以具有：

```text
binary adjacency weights
distance-decay weights
k-nearest-neighbor weights
row-standardized weights
```

因此 spatial weights **不应该永久作为 base 的固有属性**。

---

# 12. Proposed object model

最终建议：

```text
ngeo
│
├── base
│   ├── type
│   ├── elements
│   ├── geometry
│   ├── coordinate_space
│   └── topology
│
├── values
│
├── layers
│   ├── layer_id
│   ├── measure_id
│   ├── subject
│   ├── session
│   └── ...
│
├── measures
│   ├── measure_id
│   ├── name
│   ├── unit
│   ├── value_type
│   ├── support_behavior
│   └── aggregation
│
└── history
```

Optional / derived objects：

```text
support
distance
spatial_weights
transform
```

---

# 13. DK68 example

假设：

> 100 subjects × DK68 cortical thickness

### Base

```text
base:
    type = parcellation
    atlas = Desikan–Killiany
    elements = 68 cortical parcels
    coordinate_space = fsaverage
```

### Values

```text
68 × 100 matrix
```

### Layers

```text
100 rows
```

每行对应：

```text
subject
session
measure_id
```

### Measures

只有一个：

```text
measure_id = cortical_thickness
unit = mm
support_behavior = intensive
aggregation = area_weighted_mean
```

### Support

每个 DK68 parcel 实际覆盖的 cortical surface。

通常从 parcellation geometry 推导，而不是用户手工输入。

### Topology

68 个 parcels 的 cortical adjacency。

### Distance

例如：

```text
surface geodesic distance
```

### Spatial weights

例如：

```text
row-standardized adjacency matrix
```

### History

例如：

```text
FreeSurfer aparc.stats
→ read
→ reorder
→ combine hemispheres
```

---

# 14. Public API refactor

建议主要暴露以下 accessor：

```text
base(x)
values(x)
layers(x)
measures(x)
history(x)
```

由于 `base()` 在 R 语境中可能与 base package 产生概念混淆，也可以采用：

```text
spatial_base(x)
```

作为函数名，同时对象内部 slot 使用：

```text
x$base
```

这是我更推荐的方案：

```text
Internal field     Public accessor
-----------------------------------
base               spatial_base()
values             values()
layers             layers()
measures           measures()
history             history()
```

这样：

- 用户概念仍然叫 **base**；
- API 不会出现过于泛化的 `base()`。

---

# 15. Constructor API

新的 constructor 应尽可能简单。

核心 constructor 只要求：

```text
base
values
layers
measures
```

其中：

```text
layers
measures
```

可以在简单情况下自动生成。

例如用户只有：

```text
DK68 coordinates / labels
+
68 thickness values
```

不应该要求用户理解：

```text
support
distance
weights
history
transform
```

这些信息应当：

- 自动推断；
- 缺失时保持 unknown；
- 分析需要时再验证。

---

# 16. Capability-based validation

这是此次 refactor 中非常重要的一点。

不要要求每个 ngeo 对象从创建时就具有完整 metadata。

应该改为：

> different operations require different spatial capabilities.

例如：

### Plot

需要：

```text
geometry
```

### Moran's I

需要：

```text
spatial_weights
```

### Geodesic analysis

需要：

```text
surface geometry + topology
```

### Change of support

需要：

```text
support
+
measure aggregation semantics
```

### Cross-space mapping

需要：

```text
coordinate_space
+
transform
```

因此 validation 应发生在操作层，而不是 constructor 强制要求全部 metadata。

---

# 17. Backward compatibility

如果 neurogeo 当前仍处于早期开发阶段，建议 **现在进行一次明确的 breaking refactor**，而不是长期维护两套术语。

主要迁移：

```text
domain       → base
maps         → layers
provenance   → history
space        → coordinate_space
metric       → distance_method
weights      → spatial_weights
```

保留：

```text
values
measures
support
topology
transform
```

---

## 不需要Temporary compatibility layer

因为目前就是在开发阶段

文档、教程和新代码全部只使用新术语。

---

# 18. Serialized objects

不做migration 因为我们在开发 不需要考虑migration 默认用户都用最新版本

---

# 19. Documentation refactor

这是本次 refactor 最重要的工作之一。

## Getting Started 不再从 ontology 开始

不要再首先介绍：

```text
domain
support
space
topology
metric
weights
measurement semantics
transform
provenance
```

用户第一次看到 neurogeo，只需要理解：

```text
base
layer
value
measure
```

---

## Recommended opening

教程开头建议直接写：

> neurogeo represents neuroimaging data as one or more data layers defined on a shared spatial base.

例如：

> In a DK68 dataset, the spatial base consists of 68 cortical parcels. A cortical-thickness map is one layer containing 68 values, one for each parcel. Multiple subjects or imaging measures can be stored as additional layers on the same base.

然后展示：

```text
                layer 1   layer 2   layer 3
lh_bankssts       2.51      2.47      2.63
lh_caudalACC      2.74      2.82      2.70
...
rh_insula         2.89      2.93      2.84
```

用户在这里已经可以理解整个数据模型。

---

# 20. Concepts should be introduced progressively

### Tutorial 1 — Base and layers

只讲：

```text
base
values
layers
measures
```

### Tutorial 2 — Spatial relationships

再讲：

```text
topology
distance
spatial_weights
```

### Tutorial 3 — Spatial support and aggregation

再讲：

```text
support
change of support
aggregation semantics
```

### Tutorial 4 — Coordinate spaces

再讲：

```text
coordinate_space
transform
```

### Advanced / reproducibility

最后讲：

```text
history
checksums
provenance internals
```

---

# 21. Implementation phases

## Phase 1 — Terminology and object schema

优先完成：

```text
domain → base
maps → layers
provenance → history
```

并统一 accessor、print、summary 和 documentation。

不改变分析算法。

---

## Phase 2 — Simplify constructors

让普通用户能够通过：

```text
geometry + values
```

创建对象。

自动生成：

```text
base
layers
measures
history
```

能推断的信息自动推断，不能推断的保持 unknown。

---

## Phase 3 — Move analysis-specific objects out of the core

明确区分：

```text
data object
vs
analysis object
```

以下不作为创建 ngeo object 的必要字段：

```text
distance
spatial_weights
transform
```

---

## Phase 4 — Formalize support

重新检查当前 support API。

明确区分：

```text
support geometry
support size
source-target support mapping
aggregation rule
```

但不要创建过多新 public classes。

原则是：

> only expose complexity when an actual operation needs it.

---

## Phase 5 — Rewrite tutorials

优先重写：

1. Getting Started；
2. Core Concepts；
3. DK68 tutorial；
4. change-of-support tutorial。

所有教程统一采用：

```text
base + layers
```

的心智模型。

---

# 22. What should NOT be refactored now

此次修改不应该扩大到：

- 重写 Moran's I；
- 重写 spatial null；
- 重写 readers；
- 重写所有 file formats；
- 创建复杂 ontology；
- 创建大量新的 S3/S4 classes；
- 强制所有对象携带完整 support；
- 强制所有 measures 明确 aggregation semantics。

这些都可以在核心模型稳定以后逐步完善。

---

# 23. Final design principle

neurogeo 的用户模型最终应当能够压缩成一句话：

> **A spatial base defines where data live; layers contain the data observed on that base; measures describe what those values mean.**

即：

> **base 决定数据在哪里，layers 是放在这个空间基座上的数据图层，measures 描述这些数值测量的是什么。**

其他空间概念围绕这三个核心展开：

```text
base
 │
 ├── support       每个元素实际代表多大空间
 ├── topology      元素如何相邻
 ├── coordinate_space
 │
 └── distance
          ↓
    spatial_weights

layers
 │
 └── values

layers
 │
 └── measure_id
          ↓
       measures

all operations
      ↓
    history
```

这应成为 neurogeo 后续 API、教程和 documentation 的统一 conceptual model。
