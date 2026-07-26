# Neuroimaging Geoinformatics：`neurogeo` R 软件开发计划

**文档性质：** Language-independent specification + R reference implementation engineering plan  
**建议包名：** `neurogeo`  
**核心规范暂名：** Neuroimaging Geoinformatics Core Specification（NGCS）  
**文档版本：** 0.1-draft  
**日期：** 2026-07-16

---

## 0. 执行摘要

本项目的目标不是开发另一个神经影像预处理工具箱，也不是简单地把 `sf`、`spdep` 或 GIS 函数包装到脑表面上。项目的核心产品是：

> **一个明确、可验证、与编程语言无关的神经影像空间数据规范，以及该规范在 R 中的参考实现。**

该规范需要统一表达：

- 数据位于什么空间域上；
- 每个数值对应什么空间支持单元；
- 使用什么坐标空间；
- 哪些位置互为邻居；
- 距离、面积和体积如何定义；
- CT、SA、label、count 等测量如何聚合；
- atlas、registration 和 resampling 如何记录；
- 哪些计算在当前对象上是合法的。

本计划建议采用以下工程决策：

1. **R-first，但 specification 与语言无关。** R 包 `neurogeo` 作为 reference implementation；未来 Python 实现遵循同一规范和 conformance tests。
2. **FSL、FreeSurfer 和 Connectome Workbench 都不是运行时依赖。** 它们只可作为可选的验证或高级后端。
3. **核心对象不继承 `sf`。** `sf` 仅作为二维 computational chart 和 GIS 互操作出口。
4. **一个对象只表示一个空间 domain 和一个与其严格对齐的数据块。** 不在首版实现任意 mixed-support、多 assay、任意 pipeline orchestration。
5. **底层使用稀疏拓扑，而不是稠密距离矩阵。** 禁止默认构造 `n × n` 全距离矩阵。
6. **CIFTI 首版采用纯 R 读取路径。** 优先复用 `cifti` 包；不使用会调用 Workbench 的 `ciftiTools::read_cifti()` 作为核心读取器。
7. **首版重点是表示、验证、邻接、距离、空间权重和守恒聚合。** GWR、kriging、SAR/CAR 等统计模型在对象语义稳定之后再加入。
8. **不新建自定义二进制文件格式。** 标准文件仍由 NIfTI、GIFTI、CIFTI、FreeSurfer formats 和 BIDS 承担；`neurogeo` 提供统一的分析对象。

按照一名资深 R 工程师全职、一名 neuroimaging/spatial-statistics 负责人投入约 0.2–0.3 FTE 的配置，达到稳定的 1.0 版本可按 **26–36 个工程周**规划。首个可用 MVP 应控制在 12–16 个工程周内。

---

# 1. 产品定位

## 1.1 一句话定义

`neurogeo` 将 cortical surface、voxel volume、subcortical structures、grayordinates、parcels 和 spatial points 表示为具有明确 **domain、geometry、support、topology、metric、space 和 measurement semantics** 的空间对象，并提供可验证的空间运算与统计接口。

## 1.2 第一性问题

软件首先必须回答以下问题：

1. **What is the spatial domain?** 皮层 mesh、三维 voxel lattice、grayordinate composite domain，还是 point set？
2. **What does one row/value represent?** vertex、voxel、parcel、surface patch、ROI 或坐标点？
3. **What is the coordinate space?** fsnative、fsaverage、fsLR、MNI、scanner space，还是未知空间？
4. **What is adjacency?** mesh edge、6/18/26 voxel connectivity、parcel shared boundary，还是用户提供的关系？
5. **What is distance?** surface graph-geodesic、三维 Euclidean、world-space voxel distance、network distance，还是其他定义？
6. **How should values aggregate?** 面积加权均值、体积加权均值、守恒求和、众数，还是禁止自动聚合？
7. **What transformations have occurred?** registration、resampling、parcellation、masking 和 projection 是否被显式记录？

如果这些问题没有被编码到数据对象中，后续算法即使运行成功，也可能在科学意义上错误。

## 1.3 目标用户

- 使用 cortical thickness、surface area、myelin、PET、gene-expression map 的研究者；
- 使用 volumetric ROI、lesion map、subcortical segmentation 的研究者；
- 使用 CIFTI grayordinates 的 HCP/fMRIPrep 用户；
- 开发 spatial null models、spatial regression、change-of-support 方法的统计学研究者；
- 希望将 `spdep`、`gstat`、`sf` 等生态用于 neuroimaging 的 R 用户。

---

# 2. 明确的范围与非目标

## 2.1 MVP 必须完成

MVP 必须支持：

- 从普通矩阵/数组构建 surface、volume、points 和 grayordinates 对象；
- 读取 NIfTI、GIFTI、CIFTI、FreeSurfer surface/annot/curv/MGH/MGZ；
- 保存并验证坐标空间、元素索引、结构标签和数据对齐；
- surface mesh adjacency；
- voxel 6/18/26 adjacency；
- grayordinate block topology；
- vertex area、voxel volume 和区域 support size；
- pairwise/source-target distance；
- 稀疏 spatial weights；
- partition/parcellation 和语义感知聚合；
- 与 `sf`、`spdep`、`igraph` 的有限互操作；
- 清晰的错误、warning 和 capability diagnostics。

## 2.2 MVP 明确不做

首版不做：

- raw MRI preprocessing；
- skull stripping、segmentation、surface reconstruction；
- nonlinear registration；
- 替代 FreeSurfer、FSL、ANTs 或 Workbench；
- 完整的 BIDS pipeline manager；
- tractography、streamlines 和完整 connectome 数据模型；
- 深度学习；
- 高级 3D viewer；
- 任意数量 domain 混在同一对象；
- 自动构造全脑 `n × n` 距离矩阵；
- 自动猜测所有测量的生物学语义；
- 一次性移植所有 GIS 算法；
- 自定义 `.neurogeo` 二进制格式。

## 2.3 设计边界

`neurogeo` 的工作流从已经产生的空间对象开始：

```text
processed neuroimaging files
        ↓
validated neurogeospatial domain + aligned values
        ↓
geometry / topology / weights / aggregation
        ↓
spatial statistics and new methods
```

---

# 3. Language-independent specification：NGCS 0.1

## 3.1 规范词汇

本文使用：

- **MUST**：实现必须满足，否则对象无效或不符合规范；
- **SHOULD**：原则上应满足，偏离时必须有明确理由；
- **MAY**：可选扩展。

NGCS 0.1 是**语义和行为规范**，不是新的磁盘格式。不同语言可使用不同内存结构，但必须满足相同 invariants 和 conformance tests。

## 3.2 最小顶层模型

一个符合规范的数据集包含六个组成部分：

```text
NGDataset
├── domain       # 空间元素、geometry、topology recipe、space
├── values       # 与 domain 元素严格对齐的数据块
├── maps         # values 非空间轴的元数据
├── measures     # 每个 map 的测量语义
├── labels       # 可选 label tables
└── provenance   # 输入、转换和操作记录
```

### 核心约束

1. 一个 `NGDataset` **MUST** 只有一个 active spatial domain。
2. `values` 的第一维 **MUST** 与 domain 中元素的顺序一一对应。
3. 所有元素 **MUST** 具有稳定、唯一的 `element_id`。
4. 文件源索引 **MUST** 与语言内部索引分开保存。
5. 不同 spatial support 的数据 **MUST NOT** 在没有 mapping 的情况下拼进同一数据块。
6. 坐标空间未知时可以标记为 `unknown`，但 **MUST NOT** 默默假定为 MNI、fsaverage 或 fsLR。
7. 任何改变元素顺序、数量或空间支持的操作 **MUST** 更新 provenance。

## 3.3 Domain

`domain` 描述数据“在哪里”和“由哪些空间元素组成”。NGCS 0.1 定义五类 domain：

| Domain type | 最小元素 | MVP |
|---|---|---:|
| `surface` | vertex + triangular faces | 是 |
| `volume` | voxel lattice + affine + active voxel index | 是 |
| `points` | 2D/3D coordinates | 是 |
| `grayordinates` | ordered cortical vertices + subcortical voxels | 是 |
| `regions` | regions + membership 或 adjacency/support metadata | 是 |

未来可扩展：`curves`、`streamlines`、`networks`，但不进入 1.0 前的核心范围。

### 3.3.1 Surface domain

最小字段：

- `element_id`：每个 vertex 的稳定 ID；
- `coordinates`：一个或多个命名坐标集；
- `faces`：三角面索引；
- `structure`：例如 `CORTEX_LEFT`、`CORTEX_RIGHT`；
- `active_coordinates`：当前进行几何计算的坐标集；
- `mask`：例如 medial wall；
- `space`：坐标空间描述。

一个 surface **SHOULD** 允许多个共享拓扑的坐标集：

```text
white          anatomical
pial           anatomical
midthickness   anatomical / default metric surface
inflated       visualization
sphere         registration
flat           2D computational chart / visualization
```

每个坐标集必须记录：

- `name`；
- `dimension`：2 或 3；
- `role`：`anatomical | registration | visualization | chart`；
- `units`；
- `metric_eligible`；
- `source`。

**规则：** flat、inflated 和 sphere 坐标不能在没有用户明确指定的情况下用于原生 cortical distance 或 surface area。

### 3.3.2 Volume domain

最小字段：

- `dim`：三维 lattice 尺寸；
- `affine`：active voxel-to-world 4×4 transform；
- `voxel_index`：当前元素对应的 IJK；
- `structure`：可选 brain-structure 标签；
- `space`；
- `header_transforms`：保留 qform、sform 等源信息。

若 qform 与 sform 同时存在，实现必须：

1. 完整保留二者；
2. 记录哪个被选为 active affine；
3. 在二者明显不一致时产生 warning；
4. 不覆盖原始 header 信息。

### 3.3.3 Points domain

适用于 activation peaks、electrodes、AHBA samples、stimulation coordinates 等。

最小字段：

- `coordinates`；
- `space`；
- `element_id`；
- 可选 `structure`；
- 可选 support radius/uncertainty。

Points 默认没有 topology。KNN 或 distance-band 关系必须由显式操作产生。

### 3.3.4 Grayordinates domain

Grayordinates 是 ordered composite domain，通常包含：

- left cortical vertices；
- right cortical vertices；
- selected subcortical/cerebellar voxels。

CIFTI 文件中的 brain-model mapping 可告诉实现哪些 vertex/voxel 与矩阵索引对应，但 CIFTI 本身通常不包含 cortical vertex coordinates 和 triangle topology。因此：

- grayordinate 对象可以在未提供 surface geometry 时被读取；
- 此时对象仍可进行 map-level 运算；
- 但 geodesic、surface adjacency、surface area 等 capability 必须为 `FALSE`；
- 用户附加匹配的 surface 后，这些 capability 才被启用；
- surface vertex count 与 CIFTI metadata 不匹配时必须报错。

默认 grayordinate topology 是 block-diagonal：

```text
left surface mesh
⊕ right surface mesh
⊕ subcortical voxel adjacency
```

皮层—皮层下或跨半球边不能隐式加入；若需要，必须通过显式 relation/weights 对象提供。

### 3.3.5 Regions domain

Regions 用于 cortical parcels、subcortical ROIs、lesion clusters 等。

允许两种形式：

1. **Membership-backed regions**：保存 base domain 到 region 的 membership；
2. **Standalone regions**：只有 region ID、centroid、area/volume 和用户提供 adjacency。

Membership 使用稀疏矩阵或整数 label vector 表示，不复制完整 base geometry。

## 3.4 Element indexing

每个 domain 必须包含元素表：

| 字段 | 必需 | 说明 |
|---|---:|---|
| `element_id` | 是 | 对象内部稳定唯一 ID，不随 subset 外的重排而静默变化 |
| `source_index` | 建议 | 文件中的 vertex/voxel/grayordinate index |
| `source_index_base` | 建议 | 0 或 1 |
| `structure` | 建议 | cortex left/right、thalamus 等 |
| `included` | 建议 | mask 后是否参与分析 |
| `component_id` | 可选 | disconnected component 或 hemisphere |

语言实现可以使用本地索引，但任何导入/导出都必须正确转换并保留源 index base。

## 3.5 Values 与 maps

MVP 使用一个数据块：

```text
values: n_element × n_map
```

`values` 可以是：

- numeric matrix；
- integer matrix；
- logical matrix；
- 稀疏 matrix；
- `NULL`（geometry-only object）。

首版不实现任意多 assay container。不同尺寸或不同 spatial domain 的数据应放在不同对象中。

`maps` 是长度为 `n_map` 的 metadata table，例如：

- map name；
- subject/session；
- time；
- condition；
- contrast；
- source frame；
- units；
- intent。

对于 4D NIfTI，第四维成为 maps；对于 CIFTI dtseries，时间轴成为 maps；对于 dscalar，每一列成为 map。

## 3.6 Measurement semantics

每个 map 必须有 measurement metadata。最小字段：

| 字段 | 示例 |
|---|---|
| `value_type` | continuous、integer、label、probability、vector |
| `spatial_semantics` | intensive、extensive、count、categorical、unknown |
| `units` | mm、mm²、mm³、z、a.u. |
| `missing_policy` | preserve、exclude |
| `default_aggregation` | area_weighted_mean、sum、mode、none |

建议规则：

| 数据 | 典型 semantics | 默认聚合 |
|---|---|---|
| cortical thickness | intensive | vertex-area weighted mean |
| PET/receptor density | intensive | support-weighted mean |
| vertex/parcel surface area | extensive | sum |
| regional volume | extensive | sum |
| lesion/event count | count | sum |
| atlas labels | categorical | mode 或显式规则 |
| unknown scalar | unknown | 禁止自动聚合 |

软件不能只根据列名强行推断语义。可提供受控 helper，例如：

```r
ngeo_measure("cortical_thickness")
ngeo_measure("surface_area")
ngeo_measure("categorical_label")
```

若 semantics 为 `unknown`，`ngeo_aggregate()` 必须要求用户显式提供 `fun`。

## 3.7 Topology

Topology 描述直接连接，不等同于坐标距离。

### Surface

由 triangular faces 产生 undirected vertex-edge graph。实现必须检查：

- face index 是否越界；
- 是否存在重复 vertex 的退化三角形；
- 是否存在重复 faces；
- isolated vertices；
- connected components；
- 非流形边作为 diagnostic，而非一律拒绝。

### Volume

支持：

- 6-neighbor：默认；
- 18-neighbor；
- 26-neighbor。

邻接必须受 active mask 限制。

### Regions

可由以下方式产生：

- base mesh edge 跨 region 计数；
- shared boundary length/area；
- centroid distance；
- 用户提供 edge list。

### Grayordinates

默认只在每个 component 内建立 topology；跨 component 关系需要显式添加。

## 3.8 Metric

Metric 是具名、带参数的距离定义，不应隐藏在 geometry 内。

MVP 支持：

| Metric | Domain | 首版方法 |
|---|---|---|
| `euclidean` | points/surface coordinates | 直接坐标距离 |
| `world_euclidean` | volume | affine 后三维距离 |
| `edge_geodesic` | surface | mesh-edge shortest path |
| `topological_hops` | any topology | graph hop count |
| `region_centroid` | regions | centroid distance |

`edge_geodesic` 必须明确命名，不能在文档中把它夸大为连续曲面的 exact geodesic。Heat method 或 exact polyhedral geodesic 可进入后续版本。

距离接口默认只允许：

- 指定 pairs；
- sources 到 targets；
- 给定半径内邻居；
- K nearest neighbors。

除非 `n` 很小且用户显式请求，否则禁止构造全 all-pairs matrix。

## 3.9 Spatial weights

`NGWeights` 是独立对象，包含：

- 稀疏矩阵；
- 来源 domain hash；
- construction method；
- metric；
- threshold/K/bandwidth；
- symmetry；
- diagonal policy；
- normalization style；
- disconnected-component diagnostics。

核心 weights 类型：

- binary contiguity；
- shared-boundary weights；
- inverse distance；
- Gaussian kernel；
- KNN；
- row-standardized variants。

Weights 必须可转换为 `spdep::listw`，但其核心表示不能依赖 `listw`。

## 3.10 Partition 与 change of support

`NGPartition` 表示 base elements 到 regions 的 mapping：

```text
base domain elements → parcel/ROI IDs
```

最小字段：

- base domain hash；
- membership；
- region table；
- unlabeled/background policy；
- overlap policy；
- provenance。

MVP 支持 crisp partition；probabilistic/overlapping membership 延后。

聚合必须根据 measurement semantics 和 support weights 执行。输出必须记录：

- 输入 domain；
- partition；
- aggregation rule；
- excluded elements；
- missing-data handling；
- support size。

## 3.11 Space

`NGSpace` 的最小字段：

| 字段 | 说明 |
|---|---|
| `space_id` | fsnative、fsaverage、fsLR、MNI152... 或 unknown |
| `kind` | surface、volume、hybrid、unknown |
| `units` | 通常 mm |
| `structure` | 可在 domain 或 element level 保存 |
| `template` | 可选 |
| `density` | 例如 32k |
| `resolution` | 例如 2 mm |
| `source_metadata` | 原始 header/BIDS metadata |

`space_id` 相同并不自动意味着 element-wise correspondence 相同；实现还必须比较 vertex count、density、affine、mask 或 mapping metadata。

## 3.12 Transform 与 provenance

Transform 记录空间变换，不在 MVP 内负责估计 registration。

最小字段：

- source space；
- target space；
- transform type；
- method/software；
- direction；
- interpolation/resampling rule；
- Jacobian 是否可用；
- source file/checksum；
- parameters。

Provenance 至少保存：

- source path 或 source identifier；
- 文件大小和 checksum（可配置）；
- importer 和版本；
- 读取时间；
- header 摘要；
- 产生对象的操作日志；
- 规范版本；
- package version。

默认只保存必要 provenance，不复制完整大型 header 或私密路径到公开输出；提供脱敏函数。

## 3.13 Capability model

对象可能是有效的，但缺少某些分析所需信息。例如 CIFTI 可有数据和 vertex indices，却没有 surface geometry。

因此提供：

```r
ngeo_capabilities(x)
```

返回：

- `coordinates_2d`；
- `coordinates_3d`；
- `surface_topology`；
- `voxel_affine`；
- `adjacency`；
- `surface_area`；
- `voxel_volume`；
- `geodesic`；
- `partition`；
- `labels`；
- `chart`。

算法在运行前调用 capability check，而不是在深层矩阵运算中失败。

---

# 4. R 内部对象设计

## 4.1 选择 S3，而不是 S4/R6

建议首版采用受控 S3 list classes，原因：

- 更符合 `sf`、tidyverse 和普通 R 用户的使用习惯；
- constructor/validator 足以保证 invariants；
- 更容易序列化、打印和调试；
- 避免首版承担复杂 S4 inheritance；
- 不需要 R6 的可变引用语义。

`Matrix` 的稀疏矩阵对象可以作为内部组件使用，不要求整个包采用 S4。

## 4.2 顶层对象

```r
structure(
  list(
    domain     = <ngeo_domain>,
    values     = <matrix | Matrix | NULL>,
    maps       = <data.frame>,
    measures   = <data.frame>,
    labels     = <named list>,
    provenance = <list>
  ),
  class = c("ngeo_surface", "ngeo")
)
```

所有 domain-specific 对象继承 `ngeo`：

- `ngeo_surface`；
- `ngeo_volume`；
- `ngeo_points`；
- `ngeo_grayordinates`；
- `ngeo_regions`。

## 4.3 为什么不用“一行一个 vertex 的大 tibble”作为核心

虽然 `sf` 采用 data frame + geometry list-column，但直接把 32k/164k vertices 或上百万 voxels 全部展开为复杂 list-column 会造成：

- 较高对象开销；
- 对 4D 数据不自然；
- mesh faces、affine 和 composite-domain 难以表达；
- 混合 numeric/geometry 操作复制成本高。

因此核心采用：

- 紧凑 numeric matrix；
- integer face/index matrix；
- sparse adjacency；
- 小型 metadata data.frames。

需要 tidy 分析时提供：

```r
as.data.frame(x, long = FALSE)
ngeo_as_tibble(x, maps = ...)
```

但不让长表成为唯一真实表示。

## 4.4 Domain classes

### `ngeo_surface_domain`

```r
list(
  elements = data.frame(...),
  coordinates = list(
    midthickness = matrix(n_vertex, 3),
    flat = matrix(n_vertex, 2)
  ),
  coordinate_meta = data.frame(...),
  active_coordinates = "midthickness",
  faces = integer_matrix(n_face, 3),
  space = ngeo_space(...),
  mask = logical(n_vertex)
)
```

### `ngeo_volume_domain`

```r
list(
  elements = data.frame(...),
  dim = c(nx, ny, nz),
  affine = matrix(4, 4),
  voxel_index = integer_matrix(n_element, 3),
  header_transforms = list(qform = ..., sform = ...),
  space = ngeo_space(...)
)
```

### `ngeo_points_domain`

```r
list(
  elements = data.frame(...),
  coordinates = matrix(n, 3),
  space = ngeo_space(...),
  uncertainty = NULL
)
```

### `ngeo_grayordinates_domain`

```r
list(
  elements = data.frame(...),
  components = list(
    cortex_left  = <surface index mapping + optional geometry>,
    cortex_right = <surface index mapping + optional geometry>,
    subcortex    = <voxel mapping + affine>
  ),
  space = ngeo_space(kind = "hybrid", ...)
)
```

### `ngeo_regions_domain`

```r
list(
  elements = data.frame(region_id, name, structure, ...),
  base_domain_hash = NULL_or_hash,
  membership = NULL_or_sparse_matrix,
  centroid = NULL_or_matrix,
  support_size = numeric(n_region),
  adjacency = NULL_or_sparse_matrix,
  space = ngeo_space(...)
)
```

## 4.5 独立辅助类

### `ngeo_weights`

```r
list(
  matrix = Matrix::dgCMatrix,
  domain_hash = "...",
  method = "mesh_contiguity",
  normalization = "row",
  parameters = list(...),
  diagnostics = list(...)
)
```

### `ngeo_partition`

```r
list(
  membership = integer_vector_or_sparse_matrix,
  base_domain_hash = "...",
  regions = data.frame(...),
  background = 0L,
  provenance = list(...)
)
```

### `ngeo_space`

轻量 S3 list；不实现完整 ontology engine。

### `ngeo_transform`

只记录/应用已知 transform；不估计 registration。

## 4.6 Validity levels

```r
ngeo_validate(x, level = c("basic", "strict", "scientific"))
```

- `basic`：尺寸、类型、索引范围；
- `strict`：topology、space、map 对齐、labels；
- `scientific`：measure semantics、metric compatibility、跨空间风险。

构造器自动执行 `basic`；I/O 默认执行 `strict`；论文分析建议执行 `scientific`。

---

# 5. 输入支持设计

## 5.1 统一入口

```r
read_ngeo(
  x,
  geometry = NULL,
  data = NULL,
  labels = NULL,
  surfaces = NULL,
  mask = NULL,
  maps = NULL,
  space = NULL,
  measure = NULL,
  load_data = TRUE,
  strict = TRUE,
  ...
)
```

同时保留格式专用入口，方便清晰报错：

```r
read_ngeo_nifti()
read_ngeo_gifti()
read_ngeo_cifti()
read_ngeo_freesurfer()
```

`read_ngeo()` 只负责识别并 dispatch，不在一个函数中塞入所有格式逻辑。

## 5.2 输入能力矩阵

| 输入 | R backend | 输出 domain | MVP 状态 | 外部软件 |
|---|---|---|---:|---:|
| NIfTI `.nii/.nii.gz` | `RNifti` | volume | 必须 | 无 |
| GIFTI surface `.surf.gii` | `gifti` | surface | 必须 | 无 |
| GIFTI metric/shape/func | `gifti` | attach to surface | 必须 | 无 |
| GIFTI label `.label.gii` | `gifti` | labels/partition | 必须 | 无 |
| CIFTI dscalar/dlabel/dtseries | `cifti` | grayordinates | 必须 | 无 |
| FreeSurfer surface | `freesurferformats` | surface | 必须 | 无 |
| FreeSurfer annot | `freesurferformats` | labels/partition | 必须 | 无 |
| FreeSurfer curv | `freesurferformats` | vertex map | 必须 | 无 |
| MGH/MGZ volume | `freesurferformats`/`RNifti` adapter | volume | 必须 | 无 |
| MGH/MGZ surface morphometry | `freesurferformats` + surface | surface map | 必须 | 无 |
| coordinates/faces/values | native constructor | surface/points | 必须 | 无 |
| arrays + affine | native constructor | volume | 必须 | 无 |

## 5.3 NIfTI importer

### 输入映射

- 3D data → `n_element × 1`；
- 4D data → `n_element × n_frame`；
- voxel IJK + active affine → domain；
- qform/sform → preserved metadata；
- NIfTI intent → map/measure hints；
- JSON sidecar → provenance/maps metadata（若存在）。

### API

```r
x <- read_ngeo_nifti(
  "zstat1.nii.gz",
  mask = "brain_mask.nii.gz",
  maps = NULL,
  affine = "auto"
)
```

### 行为规则

- `mask = NULL` 默认保留所有 voxels，不默默删除 0；
- 提供 `mask = "nonzero"`，但必须由用户显式请求；
- 支持只读指定 frames/maps，降低 4D 内存；
- `load_data = FALSE` 可只读 geometry/header；
- 首版只支持 in-memory values，不做 file-backed lazy array。

## 5.4 GIFTI importer

支持：

- pointset + triangle arrays；
- scalar/shape/functional arrays；
- label arrays 和 label table；
- 多 data arrays；
- surface 与 metric 分文件输入。

```r
x <- read_ngeo_gifti(
  geometry = "L.midthickness.32k_fs_LR.surf.gii",
  data = c(
    thickness = "L.thickness.32k_fs_LR.shape.gii",
    myelin = "L.myelin.32k_fs_LR.func.gii"
  ),
  labels = "L.aparc.32k_fs_LR.label.gii"
)
```

Importer 必须验证 vertex count 一致；不能依赖文件名假定同一空间。

## 5.5 CIFTI importer

### 关键策略

CIFTI 核心读取不调用 Workbench。首版优先使用 `cifti` 包解析 CIFTI matrix 和 XML mapping；`ciftiTools` 仅作为可选 interoperability backend。

原因是 `ciftiTools::read_cifti()` 当前读取路径会调用 Workbench commands，而 `cifti` 包可以在 R 中读取任意 intent 的 CIFTI。

### 支持范围

MVP：

- `.dscalar.nii`；
- `.dlabel.nii`；
- `.dtseries.nii`。

后续：

- pscalar/ptseries；
- dconn/pconn 等超大矩阵，仅 metadata/subset 支持。

### API

```r
x <- read_ngeo_cifti(
  "group_effect.dscalar.nii",
  surfaces = list(
    left = "L.midthickness.32k_fs_LR.surf.gii",
    right = "R.midthickness.32k_fs_LR.surf.gii"
  )
)
```

若未提供 surfaces：

```r
x <- read_ngeo_cifti("group_effect.dscalar.nii")
ngeo_capabilities(x)$geodesic
# FALSE
```

必须保留：

- brain-model mapping；
- vertex indices；
- voxel IJK；
- volume transform；
- structure names；
- medial-wall/excluded vertices；
- map names、labels、series timing。

## 5.6 FreeSurfer importer

### Surface + curv/annot

```r
x <- read_ngeo_freesurfer(
  geometry = "surf/lh.white",
  coordinates = list(
    pial = "surf/lh.pial",
    inflated = "surf/lh.inflated",
    sphere = "surf/lh.sphere"
  ),
  data = c(
    thickness = "surf/lh.thickness",
    area = "surf/lh.area"
  ),
  labels = "label/lh.aparc.annot",
  space = ngeo_space("fsnative", structure = "CORTEX_LEFT")
)
```

同一 topology 下的不同 surface coordinate files 必须验证 vertex/face consistency。

### MGH/MGZ ambiguity

MGH/MGZ 可能表示 volume，也可能表示 surface morphometry。`domain = "auto"` 只有在元数据和维度足够明确时才能自动判定；否则必须要求：

```r
read_ngeo_freesurfer("lh.thickness.mgz", domain = "surface", geometry = "lh.white")
```

禁止把模糊 MGH/MGZ 默默解释成 volume。

## 5.7 普通对象构造器

```r
ngeo_surface(
  coordinates,
  faces,
  values = NULL,
  maps = NULL,
  measures = NULL,
  space = ngeo_space("unknown")
)

ngeo_volume(
  values = NULL,
  dim,
  affine,
  mask = NULL,
  maps = NULL,
  measures = NULL,
  space = ngeo_space("unknown")
)

ngeo_points(
  coordinates,
  values = NULL,
  space = ngeo_space("unknown")
)

ngeo_grayordinates(
  components,
  values,
  maps = NULL,
  measures = NULL
)
```

Constructors 对输入执行严格 shape checks；任何 index 自动转换都写入 provenance。

## 5.8 BIDS 支持边界

MVP 只做：

- 读取邻近 JSON/TSV sidecars；
- 解析常用 `space-`、`hemi-`、`den-`、`res-` entities；
- 保存 BIDS source path 和 metadata。

不做：

- 全 BIDS dataset indexing；
- pipeline execution；
- derivative generation orchestration。

---

# 6. 核心功能与 API

## 6.1 构造、读取与验证

```r
read_ngeo()
as_ngeo()
ngeo_surface()
ngeo_volume()
ngeo_points()
ngeo_grayordinates()
ngeo_validate()
ngeo_repair()       # 仅可安全修复的 metadata/index 问题
```

`ngeo_repair()` 不应自动改变科学含义，例如不能自动重新配准 mismatched surfaces。

## 6.2 检查与访问

```r
ngeo_domain_type(x)
ngeo_space(x)
ngeo_elements(x)
ngeo_values(x, maps = NULL)
ngeo_maps(x)
ngeo_measures(x)
ngeo_labels(x)
ngeo_provenance(x)
ngeo_capabilities(x)
```

## 6.3 Surface/volume geometry

```r
ngeo_coordinates(x, set = "active")
ngeo_set_active_coordinates(x, "midthickness")
ngeo_faces(x)
ngeo_affine(x)
ngeo_voxel_index(x)
ngeo_vertex_area(x, coordinates = "active")
ngeo_voxel_volume(x)
ngeo_support_size(x)
```

Vertex area 首版采用 triangle area 分配到 vertices 的明确规则，并记录 method；不同方法不能混为一谈。

## 6.4 Topology 与邻接

```r
ngeo_adjacency(
  x,
  method = c("mesh", "voxel", "region"),
  connectivity = 6,
  include_masked = FALSE
)

ngeo_components(x)
ngeo_boundary(x, partition = NULL)
```

返回稀疏 `Matrix` 或 `ngeo_weights`，不返回 dense matrix。

## 6.5 距离

```r
ngeo_distance(
  x,
  from,
  to = NULL,
  metric = c("edge_geodesic", "euclidean", "world_euclidean", "hops"),
  max_distance = Inf
)

ngeo_neighbors(
  x,
  method = c("contiguity", "knn", "distance_band"),
  k = NULL,
  threshold = NULL,
  metric = NULL
)
```

首版 shortest path 可以基于 sparse graph/Dijkstra；Heat method 进入后续版本。

## 6.6 Spatial weights

```r
w <- ngeo_weights(
  x,
  method = "mesh_contiguity",
  style = "W",
  kernel = NULL,
  bandwidth = NULL
)

as_spdep_nb(w)
as_spdep_listw(w)
as_igraph(w)
```

`style` 命名应尽量兼容 `spdep`，但内部保留原始未标准化矩阵。

## 6.7 Partition 与聚合

```r
p <- ngeo_partition(
  x,
  labels = "aparc",
  background = 0
)

regional <- ngeo_aggregate(
  x,
  partition = p,
  maps = c("thickness", "area")
)
```

预期行为：

- thickness → area-weighted mean；
- surface area → sum；
- unknown → error，要求 `fun=`；
- label → mode/tie policy；
- 输出 regions domain，并保留 membership provenance。

## 6.8 二维 computational chart 与 `sf`

```r
flat <- ngeo_set_chart(x, coordinates = flat_xy)
sf_obj <- ngeo_as_sf(flat, feature = "vertex")
```

规则：

- `as_sf()` 只在 2D chart 或明确的 planar geometry 上工作；
- `sf` geometry 只用于展示、平面 overlay 或明确允许的局部运算；
- 原始 area、adjacency 和 geodesic 不被二维坐标替换；
- 导出的 `sf` 对象附带指向原 domain 和 distortion metadata 的属性。

## 6.9 基础可视化

MVP 只做 diagnostic plotting：

```r
plot(x, map = "thickness")
plot(w)
plot(p)
```

实现原则：

- volume：正交切片或简单 mosaic；
- surface：flat chart 优先；可选 `rgl` 3D；
- grayordinates：左右 surface + subcortical slices 的基础布局；
- 不在首版开发完整 viewer 或 publication-theme 系统。

---

# 7. 经典算法移植计划

算法开发必须晚于对象、topology 和 weights 稳定。

## 7.1 Tier 1：直接可移植

首批实现：

- global Moran’s I；
- Geary’s C；
- local Moran/LISA；
- Getis–Ord Gi*；
- spatial correlogram；
- basic semivariogram。

策略：

- 优先复用 `spdep`/`gstat` 的成熟实现；
- `neurogeo` 负责生成科学上正确的 weights、mask 和 support；
- 只有在现有实现不支持关键语义时，才写 reference implementation。

## 7.2 Tier 2：需要 geometry adaptation

后续：

- geodesic kernel regression；
- manifold-aware GWR/MGWR；
- cortical hotspot detection；
- wombling/boundary detection；
- surface kriging；
- point processes on cortical surfaces。

## 7.3 Tier 3：项目的原创方法

1. parcellation-aware change of support；
2. parcellation-invariant inference；
3. geometry–connectivity dual weights；
4. cortical–subcortical hybrid spatial models；
5. transform uncertainty propagation；
6. multiscale cross-atlas comparison。

Tier 3 应作为论文创新主线；Tier 1 主要证明基础设施正确。

---

# 8. 包架构与依赖策略

## 8.1 单包起步

首版只维护一个主包 `neurogeo`。不要一开始拆成 `neurogeoCore`、`neurogeoIO`、`neurogeoStats` 等多个包。

达到以下条件后才考虑拆包：

- CRAN 依赖或编译时间成为实际问题；
- I/O 和统计模块有独立维护团队；
- API 已稳定且存在明确用户群。

## 8.2 建议依赖

### Hard Imports

保持精简：

- `Matrix`：稀疏拓扑和 weights；
- `Rcpp`：性能关键 primitive；
- `cli`：结构化错误和 diagnostics；
- `vctrs`：可选，用于稳定的小型 S3 metadata vectors；
- `digest`：domain/provenance hash；
- `jsonlite`：sidecar 和 provenance。

`tibble`/`rlang` 是否作为 Imports 取决于 API prototype；可先不用完整 tidyverse。

### Optional I/O dependencies

放入 `Suggests`，按需检查：

- `RNifti`；
- `gifti`；
- `cifti`；
- `freesurferformats`。

用户调用对应读取器时，若依赖缺失，提供精确安装提示。

### Optional analysis/interop dependencies

- `sf`；
- `spdep`；
- `gstat`；
- `igraph`；
- `Rvcg`；
- `rgl`；
- `ggplot2`。

### External binaries

以下均不得成为 core dependency：

- FreeSurfer；
- FSL；
- Connectome Workbench；
- ANTs。

可以提供：

```r
ngeo_backend_available("workbench")
```

但核心读取、构造、邻接、距离、weights 和统计不能调用这些程序。

## 8.3 C++ 使用边界

首版只将以下内容放入 Rcpp/C++：

- faces → unique edges；
- triangle/vertex area；
- voxel neighbor index；
- sparse graph primitive；
- 大型 mapping/aggregation kernels。

文件格式读取优先使用成熟 R 包，不自行重写 parser。

## 8.4 建议目录结构

```text
neurogeo/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── class-ngeo.R
│   ├── class-domain.R
│   ├── class-space.R
│   ├── class-weights.R
│   ├── class-partition.R
│   ├── constructors.R
│   ├── validate.R
│   ├── capabilities.R
│   ├── io.R
│   ├── io-nifti.R
│   ├── io-gifti.R
│   ├── io-cifti.R
│   ├── io-freesurfer.R
│   ├── geometry-surface.R
│   ├── geometry-volume.R
│   ├── topology.R
│   ├── distance.R
│   ├── weights.R
│   ├── partition.R
│   ├── aggregate.R
│   ├── interop-sf.R
│   ├── interop-spdep.R
│   └── plot.R
├── src/
│   ├── mesh_edges.cpp
│   ├── surface_area.cpp
│   ├── voxel_neighbors.cpp
│   └── aggregate.cpp
├── inst/
│   ├── extdata/
│   └── schema/
├── tests/testthat/
├── vignettes/
├── man/
├── README.Rmd
├── NEWS.md
├── CONTRIBUTING.md
├── CODE_OF_CONDUCT.md
└── design/
    ├── NGCS-0.1.md
    └── adr/
```

## 8.5 Architecture Decision Records

所有关键决策应写入 `design/adr/`：

- ADR-001：S3 object model；
- ADR-002：one domain + one data block；
- ADR-003：CIFTI pure-R importer；
- ADR-004：no dense distance matrices；
- ADR-005：measurement semantics；
- ADR-006：`sf` as interoperability layer, not core geometry。

---

# 9. 开发路线图

以下估算假设一名资深 R/C++ 工程师全职，领域负责人持续进行 API/scientific review。

## Phase 0：规范与 proof of concept

**工期：2–3 周**

交付：

- NGCS 0.1 draft；
- 6 个核心 ADR；
- 三个 conformance fixtures；
- surface/volume constructors prototype；
- API notebook；
- package skeleton 和 CI。

Exit criteria：

- 规范能完整表达 surface、volume、grayordinates；
- 示例对象无歧义；
- 团队冻结 MVP/non-goals；
- 不存在必须依赖 FSL/FreeSurfer/Workbench 的设计。

## Phase 1：核心对象和原生 constructors

**工期：4–5 周**

交付：

- `ngeo` S3 classes；
- surface/volume/points/grayordinates/regions domains；
- values/maps/measures；
- basic/strict validation；
- capability system；
- print/summary/subset；
- raw constructors。

Exit criteria：

- element indexing 在 subset/reorder 后正确；
- values 与 domain 永远同步；
- invalid faces、affine 和 map sizes 能提前失败；
- 100% synthetic tests 通过。

## Phase 2：I/O adapters

**工期：6–8 周**

交付顺序：

1. NIfTI；
2. GIFTI；
3. FreeSurfer surface/curv/annot；
4. MGH/MGZ；
5. CIFTI dscalar/dlabel/dtseries。

Exit criteria：

- 所列格式均能转换成同一 object semantics；
- 不安装外部软件也能运行测试；
- format metadata 和 source indices 被保留；
- CIFTI 无 surfaces 时 capability 正确降级；
- CIFTI surfaces mismatch 会 fail fast。

## Phase 3：Geometry、topology、distance、weights

**工期：6–7 周**

交付：

- mesh adjacency；
- voxel adjacency；
- component diagnostics；
- vertex area/voxel volume；
- edge-geodesic/world Euclidean；
- KNN/distance-band；
- `ngeo_weights`；
- `spdep`/`igraph` converters。

Exit criteria：

- 不产生隐式 dense `n × n` 对象；
- 32k surface 能在普通笔记本上稳定处理；
- adjacency 与 reference outputs 一致；
- no cross-hemisphere edges by default。

## Phase 4：Partition 与语义感知聚合

**工期：4–5 周**

交付：

- GIFTI/annot/dlabel → partition；
- surface/volume → regions；
- area/volume weighted aggregation；
- extensive conservation tests；
- region adjacency；
- provenance。

Exit criteria：

- surface area 聚合守恒；
- thickness 的 weighted mean 可复现 reference；
- unknown semantics 无显式 `fun` 时拒绝运行；
- atlas background/medial wall policy 可追踪。

## Phase 5：经典空间统计与文档

**工期：4–6 周**

交付：

- Moran’s I；
- LISA；
- Geary’s C；
- basic variogram；
- diagnostic plotting；
- 四篇 vignettes；
- pkgdown site；
- benchmarks；
- CRAN/Bioconductor 路线决策。

Exit criteria：

- 结果与 `spdep` reference 一致；
- permutation tests 可复现；
- 用户从任一支持格式到 Moran’s I 的完整 workflow 文档化；
- R CMD check 在 Linux/macOS/Windows 通过。

## Phase 6：1.0 stabilization

**工期：3–4 周**

交付：

- API freeze；
- specification 1.0；
- deprecation policy；
- conformance suite；
- performance regression tests；
- reproducible release archive；
- software paper preprint。

---

# 10. 前 90 天的具体 backlog

## 第 1–2 周

- 创建 GitHub repository；
- package skeleton；
- 写 NGCS 0.1；
- 冻结 top-level object fields；
- 创建 tiny tetrahedral/icosahedral surface fixture；
- 创建 3×3×3 affine volume fixture；
- 创建 tiny hybrid grayordinate fixture。

## 第 3–6 周

- `ngeo_surface()`、`ngeo_volume()`、`ngeo_points()`；
- maps/measures；
- validators；
- subset/reorder；
- print/summary；
- domain hashing；
- provenance minimal implementation。

## 第 7–9 周

- NIfTI importer；
- GIFTI importer；
- FreeSurfer surface/curv/annot importer；
- format-specific golden tests。

## 第 10–13 周

- CIFTI importer；
- grayordinate components；
- surface attachment；
- mesh adjacency；
- voxel adjacency；
- `ngeo_weights()` prototype。

90 天后应该出现一个能真实演示的 MVP：

```r
x <- read_ngeo("group.dscalar.nii", surfaces = ...)
w <- ngeo_weights(x, method = "component_contiguity")
summary(x)
summary(w)
```

但此时不急于加入 GWR、kriging 或复杂 viewer。

---

# 11. 测试与科学验证

## 11.1 测试层级

### Unit tests

- constructors；
- index conversion；
- validation；
- area/volume；
- adjacency；
- aggregation；
- label tables。

### Conformance tests

语言无关 fixtures：

1. 小型 triangular surface；
2. 小型 masked volume；
3. 小型 grayordinate composite；
4. 一个 partition；
5. expected adjacency/area/distance/aggregation outputs。

未来 Python 实现必须通过同一 expected outputs。

### Golden I/O tests

使用可公开分发的小型文件，检查：

- coordinates；
- faces；
- affine；
- values；
- labels；
- indices；
- space metadata。

### Optional integration tests

在独立 Docker/nightly job 中，与以下工具对照：

- FreeSurfer surface area；
- Workbench vertex area/geodesic；
- FSL/Workbench affine metadata；
- `spdep` statistics。

这些测试不能成为用户安装依赖，也不能进入普通 CRAN checks。

## 11.2 关键 scientific invariants

### Surface

- `nrow(values) == n_vertex`；
- face indices 合法；
- area 非负；
- component/hemisphere 不被意外连接；
- flat coordinates 不替换 native metric。

### Volume

- IJK ↔ world transform 可逆到数值容差；
- voxel volume 等于 affine determinant 的绝对值；
- mask 与 values 对齐；
- qform/sform conflict 被报告。

### Grayordinates

- global row order 与 CIFTI mapping 一致；
- vertex indices 和 voxel IJK 保留；
- surface geometry optional；
- structure counts 一致；
- cortical/subcortical topology 分块正确。

### Aggregation

- extensive quantities 总量守恒；
- intensive quantities 使用正确 weights；
- background/NA policy 可重复；
- region support sizes 正确。

## 11.3 数值容差

每个算法必须定义：

- absolute tolerance；
- relative tolerance；
- floating-point platform differences；
- deterministic seed policy。

## 11.4 性能要求

不在设计阶段承诺不现实的毫秒级指标，但设置以下硬规则：

- topology storage 必须为 `O(n + e)`；
- 不允许默认 all-pairs distance；
- 32k surface 是日常测试规模；
- 164k surface 是 benchmark 规模；
- 91k grayordinates 是 release benchmark；
- 大型 CIFTI 必须允许 map/frame subset 或 metadata-only 读取；
- performance regressions 超过预设比例时 CI 报警。

---

# 12. 错误处理与用户体验

## 12.1 Fail early

示例：

```text
Surface data has 32,492 values but geometry has 163,842 vertices.
No implicit resampling was performed.
Provide a matching surface or an explicit mapping.
```

而不是：

```text
subscript out of bounds
```

## 12.2 Warning 分类

- `ngeo_warning_space_unknown`；
- `ngeo_warning_transform_conflict`；
- `ngeo_warning_measure_unknown`；
- `ngeo_warning_disconnected_topology`；
- `ngeo_warning_projection_metric`。

结构化 warning 便于测试和用户捕获。

## 12.3 自动行为限制

禁止以下隐式行为：

- 自动 resample mismatched meshes；
- 自动把未知坐标当 MNI；
- 自动连接左右半球；
- 自动把 0 当 background；
- 自动把任何 numeric map 当 intensive；
- 自动把 flattened distance 当 cortical distance。

---

# 13. 文档计划

MVP 至少提供四篇 vignettes：

1. **Core concepts**：domain、support、space、topology、metric；
2. **Reading data**：NIfTI/GIFTI/CIFTI/FreeSurfer；
3. **Neighbors and weights**：surface/volume/grayordinates；
4. **Parcellation and aggregation**：CT vs SA semantics。

另提供：

- 术语表；
- supported formats table；
- algorithm capability matrix；
- “common mistakes” 页面；
- reproducible examples；
- developer specification；
- conformance fixture documentation。

---

# 14. 发布、治理和兼容性

## 14.1 Versioning

- package 遵循 semantic versioning；
- specification 单独 version；
- object 内保存 `spec_version`；
- 1.0 前允许 breaking changes，但必须写 migration notes；
- 1.0 后 deprecated API 至少保留两个 minor releases。

## 14.2 CRAN 还是 Bioconductor

建议开发早期使用 GitHub + r-universe；达到稳定后评估：

- **CRAN**：更接近 `sf/spdep` 用户，安装简单；
- **Bioconductor**：适合正式 biological data container 和 experiment ecosystem。

首版不要同时维护两套发布渠道。若选择 CRAN，仍可与 Bioconductor objects 提供 converters。

## 14.3 License

优先考虑 MIT；但在正式发布前必须完成 dependency/license audit：

- 不复制 GPL package 源码；
- 不 vendor FreeSurfer/Workbench components；
- adapters 通过公开 API 调用依赖；
- 测试数据具有明确再分发许可。

## 14.4 Governance

- 所有 major API changes 走 issue + ADR；
- specification changes 需要 scientific reviewer 和 engineering reviewer；
- 至少两名 maintainer 拥有 release 权限；
- 用 public roadmap 管理功能优先级；
- feature request 必须说明对应 domain/support/metric 语义。

---

# 15. 风险登记表

| 风险 | 影响 | 缓解策略 |
|---|---|---|
| CIFTI 无 Workbench 读取不完整 | 高 | 先复用 `cifti`；建立 golden tests；必要时只补 parser 缺口，不重写全部格式 |
| 对象模型过度复杂 | 高 | 一个 domain + 一个 values block；不做 multi-assay；五个 domain classes 封顶 |
| 对象模型过度简单 | 中 | capability model；regions/grayordinates 作为一等 domain；规范可版本化扩展 |
| 32k/164k 数据内存过大 | 高 | 紧凑 matrix、sparse topology、subset read；禁止 dense distance |
| coordinate-space metadata 缺失 | 高 | 允许 `unknown`，但禁止隐式转换；scientific validation 警告 |
| 测量语义错误 | 高 | `unknown` 默认；语义依赖操作要求显式 metadata |
| CIFTI surface 与数据不匹配 | 高 | vertex count、structure、density 和 index mapping 检查 |
| 依赖包 API 变化 | 中 | 每个格式独立 adapter；最小化调用面；CI 测试 release/devel versions |
| 项目变成通用 neuroimaging toolbox | 高 | non-goals 固化；新功能必须与 geoinformatics core 直接相关 |
| 算法数量过多、论文主线模糊 | 高 | 1.0 只做基础算法；原创主线锁定 change of support |
| 结果与既有工具不一致 | 中 | optional golden integration tests；记录方法差异，不盲目追求 bitwise equality |
| 缺少用户采用 | 中 | ergonomic constructors；常见格式 end-to-end examples；`sf/spdep` converters |

---

# 16. 1.0 Definition of Done

`neurogeo` 1.0 只有在满足以下条件时发布：

- [ ] NGCS 1.0 完成并公开；
- [ ] surface、volume、points、grayordinates、regions 均有稳定对象；
- [ ] 所有列出的输入格式可在无外部软件环境中读取；
- [ ] NIfTI/GIFTI/CIFTI/FreeSurfer golden tests 通过；
- [ ] mesh/voxel/grayordinate topology 正确；
- [ ] area、volume、distance 和 weights 有 reference validation；
- [ ] partition 和聚合通过守恒测试；
- [ ] Moran/LISA 等基础算法与 reference 一致；
- [ ] 不存在默认 dense all-pairs 操作；
- [ ] Linux、macOS、Windows R CMD check 通过；
- [ ] 主要 API 文档和 vignettes 完成；
- [ ] 至少两个外部真实数据 workflow 成功复现；
- [ ] provenance 和 error messages 达到可审计标准；
- [ ] package 无 FSL、FreeSurfer、Workbench runtime dependency。

---

# 17. 推荐的首个公开演示

## Demo A：Cortical thickness

```r
library(neurogeo)

lh <- read_ngeo(
  geometry = "lh.midthickness.surf.gii",
  data = c(thickness = "lh.thickness.shape.gii"),
  labels = "lh.aparc.label.gii",
  measure = list(
    thickness = ngeo_measure("cortical_thickness")
  )
)

w <- ngeo_weights(lh, method = "mesh_contiguity", style = "W")
regional <- ngeo_aggregate(lh, partition = ngeo_partition(lh, "aparc"))
```

此 demo 展示：

- surface object；
- mesh topology；
- measurement semantics；
- partition；
- area-weighted aggregation。

## Demo B：CIFTI grayordinates

```r
x <- read_ngeo(
  "group_effect.dscalar.nii",
  surfaces = list(
    left = "L.midthickness.32k_fs_LR.surf.gii",
    right = "R.midthickness.32k_fs_LR.surf.gii"
  )
)

ngeo_capabilities(x)
w <- ngeo_weights(x, method = "component_contiguity")
```

此 demo 展示：

- cortical + subcortical hybrid domain；
- surface geometry attachment；
- block topology；
- 无 Workbench 运行时依赖。

## Demo C：NIfTI subcortical map

```r
v <- read_ngeo(
  "subcortical_effect.nii.gz",
  mask = "subcortical_mask.nii.gz"
)

w <- ngeo_weights(v, method = "voxel_contiguity", connectivity = 6)
```

此 demo 证明项目不是 cortical-only。

---

# 18. 立即执行的下一步

按优先级：

1. 冻结本文中的 MVP/non-goals；
2. 将 NGCS 单独拆为 `design/NGCS-0.1.md`；
3. 创建三个 tiny conformance fixtures；
4. 用 200–300 行 R prototype 实现 `ngeo_surface()` 和 `ngeo_validate()`；
5. 用真实 GIFTI surface + metric 验证对象结构；
6. 再实现 `ngeo_volume()`；
7. 在开始 CIFTI importer 之前冻结 element/index conventions；
8. 在任何经典算法之前完成 `ngeo_weights` 和 measurement semantics。

最重要的工程纪律是：

> **每加入一个算法之前，先证明它使用的 domain、support、topology、metric 和 aggregation semantics 已被对象明确表达。**

---

# 19. 参考资料与技术依据

1. NIfTI Data Format Working Group. **NIfTI-1 Data Format**. https://nifti.nimh.nih.gov/nifti-1/
2. NIfTI FAQ：coordinate transforms and orientation. https://nifti.nimh.nih.gov/nifti-1/documentation/faq.html
3. Human Connectome Project. **CIFTI organizational concepts / `-cifti-help`**. https://www.humanconnectome.org/software/workbench-command/-cifti-help
4. CIFTI-2 specification：matrix mapping、brain models 以及 surface geometry 不嵌入 CIFTI 的设计。https://www.nitrc.org/projects/cifti/
5. FreeSurfer Wiki. **File Formats**. https://surfer.nmr.mgh.harvard.edu/fswiki/FileFormats
6. BIDS Specification. **Imaging data types and surface/combined segmentations**. https://bids-specification.readthedocs.io/en/stable/derivatives/imaging.html
7. `sf`：Simple Features data-frame/list-column model and CRS. https://r-spatial.github.io/sf/articles/sf1.html
8. `RNifti`：NIfTI-1/NIfTI-2 read/write and affine utilities. https://cran.r-project.org/package=RNifti
9. `gifti`：R GIFTI reader. https://cran.r-project.org/package=gifti
10. `freesurferformats`：FreeSurfer surface、annot、curv、MGH/MGZ read/write without FreeSurfer installation. https://cran.r-project.org/package=freesurferformats
11. `cifti`：pure-R CIFTI reader. https://cran.r-project.org/package=cifti
12. `ciftiTools`：CIFTI/grayordinate high-level interface；其部分读取/处理路径调用 Connectome Workbench。https://cran.r-project.org/package=ciftiTools
13. `Matrix`：sparse/dense matrix classes. https://cran.r-project.org/package=Matrix
14. `vctrs`：stable S3 vector behavior. https://vctrs.r-lib.org/

---

## 最终建议

本项目最现实、最有长期价值的路线不是先做很多算法，而是先交付一个足够小、但科学语义完整的空间对象：

```text
one domain
+ aligned values
+ explicit space
+ explicit topology
+ explicit metric
+ explicit measurement semantics
+ auditable provenance
```

只要这一层设计正确，后续 Moran’s I、LISA、variogram、GWR、kriging 和 change-of-support 都可以稳定扩展；如果这一层设计模糊，算法越多，错误传播越广。
