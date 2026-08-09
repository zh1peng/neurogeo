# neurogeo 6.0 全面审计与可执行改进计划

> 审计基线：`neurogeo 6.0.0`，Git commit `b5129a4`  
> 审计日期：2026-08-09  
> 文档状态：供后续开发 Agent、维护者、方法学合作者和论文团队共同执行的审计基线  
> 本文不是新的 NGCS 规范，也不表示当前版本已达到科学发布或 Nature Communications 投稿条件。

## 0. 如何使用本文

本文把仓库审计结果分成三类，避免把代码事实、尚待验证的科学风险和产品决策混在一起：

- **已核实事实**：由当前源码、规范、生成物或本轮定向运行直接支持；
- **待独立验证的科学风险**：存在明确理论或实现风险，但最终结论必须由预注册模拟、外部参考实现或真实数据验证决定；
- **建议决策**：为了降低维护成本、改善用户体验或建立发表主线而提出的路线选择。

任务优先级定义如下：

- **P0 / Release blocker**：修复前不得把 6.0 作为稳定科学发布版、论文复现版或继续扩展算法功能；
- **P1 / High**：P0 完成后立即处理，决定产品是否可供目标用户可靠使用；
- **P2 / Medium**：支撑长期维护、规模化验证和正式发布；
- **P3 / Later**：不阻断当前稳定化，但应防止继续累积。

后续 Agent 应先阅读第 11 节的执行协议，再领取一个任务 ID。不得把“测试通过”自动解释为“科学有效”，也不得在没有证据的情况下把风险写成已经证实的方法缺陷。

---

## 1. 执行摘要

### 1.1 一句话结论

`neurogeo` 已经具备一个有辨识度的工程与方法学核心，但当前 6.0 **还不是用户可放心上手、科学主张充分验证、论文材料彼此一致的稳定发布版**。

最值得保留并继续投入的主线不是“覆盖更多空间统计函数”，而是：

> 在 surface、volume、grayordinate 与 parcellation 之间，以明确的空间身份、测量语义、物理 support、守恒约束和不确定性完成可审计的 change-of-support 与跨 support 推断。

当前最重要的动作不是增加功能，而是冻结功能面，依次完成：

1. 修复会造成错误结果、静默选错 layer 或输出丢失的 P0 缺陷；
2. 统一 6.0 的词汇、API 生命周期、教程与发布证据；
3. 用独立参考、预注册模拟和外部数据验证一条中心科学主张；
4. 只有在该主张表现出广泛领域影响时，才以 Nature Communications 为目标。

### 1.2 当前状态总览

| 维度 | 状态 | 审计判断 |
|---|---|---|
| 核心对象与不变量 | 较强 | `base / values / layers / measures / history` 心智模型、严格对齐和显式空间身份值得保留 |
| 自动化测试与跨平台检查 | 较强 | 本轮 377 个测试块通过；已有 6.0 `R CMD check` 为 `Status: OK` |
| 首次用户体验 | 阻断 | README 的第一次 Moran 分析即因旧参数失败；教程入口和版本叙事不一致 |
| 术语与 API 可理解性 | 高风险 | 226 个 exports、多套历史词汇、无前缀访问器和机械替换污染造成认知与误用风险 |
| 科学实现正确性 | 阻断 | 已复现支持加权回归、spin、Moran spectral null 等结果级缺陷；部分方法边界不足 |
| 科学验证 | 不足 | 现有小域参考验证有价值，但 6.0 重构没有绑定当前候选源码的新鲜证据，也没有独立真实研究验证 |
| 软件维护性 | 中高风险 | 验证脚本与规范重复、公开面过宽、复杂 orchestration 函数、单维护者和快速版本跃迁 |
| 论文与发布 | 阻断 | `paper/paper.md` 仍描述 NGCS 2.0，且与 6.0 实现直接矛盾；没有不可变正式 release/DOI |
| NC 级贡献潜力 | 有条件 | support-aware inference 可能形成主线；“统一工具箱”或函数数量本身不足以构成 NC 贡献 |

### 1.3 发布冻结建议

在第 8.1 节的 Phase 0 exit gate 全部满足前：

- 不新增新的统计方法、容器或 public export；
- 不宣称当前 spin、Moran spectral randomization、support-aware regression、graph-metric kriging 已通过科学验证；
- 不把 6.0 作为论文结果的唯一复现依赖；
- 不批量润色 54 篇教程，先固定词汇、API 和唯一文档来源；
- 可以继续做 P0 修复、回归测试、文档一致性和验证基础设施工作。

---

## 2. 审计范围、方法与局限

### 2.1 审计范围

本轮覆盖：

- `R/` 中的对象模型、I/O、空间关系、重采样、support、null、回归与推断实现；
- `NAMESPACE`、`.Rd`、vignettes、README、VitePress、pkgdown 和论文草稿；
- `tests/`、`tools/`、CI、release/check-output 证据、治理与引用文件；
- 与 Workbench、neuromaps、BrainSpace、BrainSMASH、ciftiTools、BrainStat、ENIGMA Toolbox 及 R 空间统计生态的定位差异；
- Nature Communications 与 Nature computational-tools reporting 的公开要求。

### 2.2 当前规模

审计时仓库大致包含：

- 65 个 R 源文件，约 31,500 行 R 代码；
- 226 个导出符号、95 个 S3 方法；
- 225 个 `.Rd` 页面，其中只有 33 个包含 examples，未发现 `details` 或 `references` 区块；
- 66 个 testthat 文件；
- 54 篇 vignette，其中中文 31 篇、英文 23 篇；
- `design/` 121 个文件，`inst/spec/` 92 个文件；两处存在大量同名但内容不同的历史规范；
- 45 个 `NEWS.md` 版本标题，而可见 Git 历史只从 2026-07-26 的 3.5.1 开始，到 2026-08-09 已为 6.0.0；没有 Git tag。

这些数量不是质量结论，但说明单维护者需要承担的 public API、文档和验证面已经非常宽。

### 2.3 本轮执行证据

本轮从当前工作区执行 6.0 单元测试：

```text
test blocks: 377
failures:    0
errors:      0
skips:       7
elapsed:     about 53 seconds
```

7 个 skip 均为需要 `NEUROGEO_FULL_PERF` 的 32k、91k、100k 或 164k 规模性能用例。已有 `check-output/rcheck-60-final/neurogeo.Rcheck/00check.log` 显示 `Status: OK`。

本轮还独立复核了以下定向探针：

- README `map = "signal"` 在 6.0 报 `unused argument`；
- 两个 layer 使用同一个 `name = "signal"` 时 strict validation 通过，字符选择静默返回第一列；
- `.ngeo_fit_support_effect()` 的结果与无权重 OLS 完全一致，并警告 `spatial_weights` 参数被忽略；
- 当前 QR spin rotation 的矩阵元素经验均值显著偏离 Haar-uniform SO(3) 的必要矩条件；
- 在一个不规则 5 节点图上，当前 Moran spectral null 改变样本均值、中心平方和与 Moran's I；
- `.ngeo_atomic_write()` 的覆盖路径确实先删除旧文件，再尝试重命名临时文件。

### 2.4 审计局限

本轮没有：

- 运行全部 `NEUROGEO_FULL_PERF` 大规模任务；
- 对所有 226 个 exports 逐个完成数学证明或独立参考比较；
- 使用外部实验室的真实数据复现研究结论；
- 进行正式安全渗透测试；
- 证明项目一定能够或不能够发表于某个期刊。

因此，第 5 节列出的“待独立验证风险”必须通过对应任务验证，不能仅凭本审计升级为论文结论。

---

## 3. 应保留的设计与资产

后续重构不应推倒重来。以下资产是项目最强的基础。

### 3.1 核心数据模型

当前五部分模型相对 5.x 更容易解释：

```text
ngeo
├── base      where observations live
├── values    element × layer values
├── layers    what each values column represents
├── measures  what the numbers mean
└── history   how the object was produced
```

其中最重要的不变量是：`values[i, j]` 与 `base$elements[i, ]`、`layers[j, ]` 严格对齐。这应成为用户教程、API 契约和论文 Methods 的共同开场，而不是先从 NGCS ontology 开始。

### 3.2 安全的空间边界

应保留：

- 不隐式配准、分割、重建或跨空间重采样；
- coordinate space、transform path 和 base identity 显式化；
- 默认不构造全脑稠密 pairwise distance matrix；
- target-by-source 的稀疏 support operator；
- extensive/count 与 intensive measurement semantics 的区分；
- classed failures、资源 guard 和 checksum-pinned 外部 fixtures。

### 3.3 已有验证基础

现有 `tools/run-scientific-validation-42.R` 已经对 Moran/Geary/Local Moran、SAR/SEM、variogram/Euclidean kriging、GWR 与 `spdep`、`spatialreg`、`gstat`、`GWmodel` 做了小域 matched-estimand 比较。这些是有价值的回归基线，应升级为 version-bound validation，而不是删除。

CI 已经覆盖 Ubuntu release/devel/oldrel、macOS、Windows、coverage、checksum-pinned real-data 和文档构建。后续应修复覆盖范围和证据绑定，而不是重新发明一套 CI。

### 3.4 教程中的可用部分

新版中文 `getting-started-zh.Rmd` 已经形成较好的教学弧线：构造点网格、检查对齐、构建权重、手算/调用 Moran、LISA、多重比较、聚合语义和检查清单。它适合作为重写其他教程的质量模板，但需要被拆成真正的 first-use 路径与概念进阶，而不是让第一次使用者一次吸收所有内容。

---

## 4. 用户体验、命名与教程审计

### 4.0 当前用户旅程基线

当前目标用户大致经历以下路径；后续 IA/教程改造必须以能否消除这些断点为基线，而不是只比较页面数量：

1. **发现项目**：首页先强调 NGCS/spec，用户难以立即判断自己手里的 NIfTI、GIFTI、CIFTI 或 ROI 矩阵能完成什么。对应 `IA-101`；
2. **安装**：README 安装可变的 GitHub `main`，用户不知道稳定版本、系统依赖和 optional backend。对应 `INSTALL-101`、`REL-201`；
3. **第一次运行**：官方 Moran 示例使用已删除的 `map=`，立即失败。对应 `DOC-001`；
4. **理解对象**：用户同时遇到 domain/maps/provenance、5.1 与 6.0 的 base/layers/measures/history。对应 `TERM-001`、`DOC-001`、`IA-101`；
5. **导入真实数据**：关键教程依赖伪路径、`eval=FALSE` 和未定义的 fixture，且直接修改 `$measures`。对应 `DATA-101`、`API-104`、`TUT-101/201`；
6. **选择分析入口**：226 个 exports 与 54 篇按内部模块排列的文章使 stable/experimental、task/method 难以区分，重名 layer 还可能静默选错。对应 `API-001/101`、`IA-101`；
7. **解释与复现**：高级能力开始体现价值，但帮助页缺少公式、assumptions、references、结果解释，发布又没有 tag/DOI。对应 `DOC-201`、`SCI-102`、`REL-201`。

用户旅程的目标状态是：从首页到第一张可解释结果不需要阅读历史规范，不需要直接修改对象内部字段，并且每一步都能知道空间、单位、support、null 与稳定性。

### 4.1 P0：官方入口无法运行

`README.md:66` 与 `website/guide/index.md:61` 仍使用：

```r
ngeo_moran(x, w, map = "signal", permutations = 999, seed = 2026)
```

6.0 的参数已经是 `layer`。这是用户复制第一段分析代码就失败的阻断问题。当前 R CMD check 不执行 README 和手写 website 页面，因此没有发现。

**整改原则**：不是只改两行，而是让所有首页、guide 和 quickstart 代码在 clean install 中由 CI 实际执行。

### 4.2 P0：layer 名歧义会静默选错数据

当前只要求 `layer_id` 唯一，不要求用户可见的 `layers$name` 唯一。`.ngeo_layer_selection()` 用 `match()` 同时匹配 ID 和 name，因此重名会静默返回第一个结果。

这不是一般 UX 瑕疵，而是可能改变 phenotype、统计量和论文结论的科学正确性问题。

**推荐契约**：

- `layer_id` 始终唯一；
- `name` 可以重复以表达同一 measure，但字符 selector 若匹配多个 name 必须报 `ngeo_error_layer_ambiguous`，并列出候选 `layer_id`；
- 分析 API 在结果中记录最终解析到的 `layer_id`，不能只记录 name；
- 若用户确实需要批量按 name 选取，应使用显式的多 layer selector，而不是单 layer API 的隐式行为。

### 4.3 P0：6.0、5.1 与历史术语同时存在

当前 canonical 6.0 规范使用 `base / values / layers / measures / history`，但手写网站仍存在：

- `website/index.md:14-23` 的 `domain`、`provenance`、`points/regions`；
- `website/concepts/index.md:2-6` 的“5.1 数据模型”；
- `website/tutorials/index.md:31` 的“教程和 API 使用 5.1”；
- `_pkgdown.yml` 中“5.0 inference surface”等历史描述；
- 多篇 vignette 中的 4.5/4.6/4.8/4.9/5.0 promotion gate 与内部开发语言。

本地生成的 `docs/` 和 `website/tutorials/` 还保留更多旧 API，但这些目录并非当前 tracked source。正确做法是从 clean checkout 生成并部署，禁止把本地旧生成物当 canonical。

### 4.4 P0：机械全局替换污染了领域术语

已核实的用户可见例子包括：

- `R/io-gifti.R:227-230` 把 GIFTI metric/shape data 写成 `distance_method`；
- `R/io-write.R` 生成相同的错误帮助文本；
- `R/weights.R` 出现 “spatial spatial_weights”；
- `R/qc.R`、`R/validate.R` 出现 “Coordinate coordinate_space”；
- `R/statistics.R` 出现 “distance distance_method”；
- 错误信息仍暴露 `prototype`、`MVP`、promotion gate 等内部里程碑语言。

这些痕迹说明 6.0 重命名曾跨越标识符、自然语言和领域专有词做了不安全的机械替换。后续不得再进行无语义边界的全局替换。

### 4.5 P1：访问器命名和冲突

当前同时存在：

- `spatial_base()`、`base_elements()`；
- `values()`、`layers()`、`measures()`、`history()`；
- `ngeo_labels()`。

`library(neurogeo)` 会遮蔽 `utils::history()`；`values()` 也容易与其他空间/影像包冲突。命名风格不一致，且用户难以从自动补全中识别哪些函数属于 `neurogeo`。

**推荐**：在 6.x 增加 `ngeo_spatial_base()`/`ngeo_base_elements()`/`ngeo_values()`/`ngeo_layers()`/`ngeo_measures()`/`ngeo_history()` 一致别名；旧名称在 6.x 维持功能，在下一 major 经过完整 deprecation 周期后再决定是否移除。不要在 6.0.x 直接破坏用户代码。

### 4.6 P1：反直觉 API

#### `ngeo_validate_layers()`

该函数并不只是验证 layers 表，而是按 independent unit 与 feature 构造 `ngeo_layer_index`。建议新增语义准确的 `ngeo_layer_index()` 或 `ngeo_build_layer_index()`，原函数作为兼容入口。

#### measures 的构造与更新

`ngeo_measure()` 不直接创建稳定的 `measure_id/name`，构造器又可能按数量与位置隐式映射。部分教程为每个 subject 重复同一 measure；`reading-data` 教程还直接修改：

```r
surface$measures$support_behavior <- "intensive"
surface$measures$unit <- "mm"
```

这绕过 validation 和 history。应提供最小、安全的 `ngeo_update_measure()`，或允许 reader/constructor 显式传入 measure，不应增加一组无边界的通用 setter。

#### 未知空间默认 `mm`

`space_id = "unknown"` 时仍可默认 `unit = "mm"`。对任意 points 或未确认的模板坐标，这会产生“数值能算、物理语义未知”的距离。unknown space 应默认 unknown unit，或所有需要物理单位的方法必须先要求用户确认。

### 4.7 P1：教程像设计记录，不像用户文档

很多高级 vignette 使用：

- version gate、promotion、endpoint、schedule；
- “Claim / non-claim”式评审备注；
- 极短的合成网格和函数调用；
- 很少的图、预期输出、解释、诊断和报告模板；
- `eval=FALSE` 与 `/path/to/...` 伪路径；
- 对 `$base/$values/$measures` 的直接访问或修改。

科学边界不应删除，但要改写为用户问题：

1. 这个方法回答什么问题？
2. 输入数据和单位是什么？
3. 前提假设是什么？
4. 怎样检查失败或误用？
5. 输出中的每个关键字段怎样解释？
6. 可以与不可以得出什么结论？
7. 论文 Methods 应报告哪些信息？

版本历史、promotion gate 和内部 nonclaim registry 应移到 NEWS/spec/developer 文档。

### 4.8 推荐的信息架构

#### 首页：按数据与研究任务进入

- 我有 NIfTI volume；
- 我有 GIFTI/FreeSurfer surface；
- 我有 CIFTI grayordinates；
- 我有 ROI × subject 矩阵；
- 我需要聚合/change of support；
- 我需要空间自相关、group inference 或可复现导出。

首页只保留：一句价值主张、当前稳定版本、15 分钟真实 quickstart、一张对象模型图和稳定/实验状态入口。

#### Start here

1. 安装与环境诊断；
2. 选择 reader；
3. 读取并检查空间与 measurement semantics；
4. `validate` 与 QC；
5. 第一个分析与图；
6. 查看 history 并选择下一教程。

#### Concepts

- base、element、values、layer、measure；
- identity 与 alignment；
- coordinate space / transform / registration / resampling；
- topology / distance / spatial weights；
- partition / parcellation / support map / aggregation；
- intensive / extensive / categorical 等 measurement semantics；
- inference unit、null、support family；
- 神经影像格式与 NGCS 术语 crosswalk。

#### Tutorials

| 编号 | 教程 | 必须执行的主线 |
|---|---|---|
| T00 | 安装与环境诊断 | stable/tagged install、optional dependencies、session info |
| T01 | 15 分钟第一个真实工作流 | fixture → read → inspect → validate/QC → analysis → plot → history |
| T10 | NIfTI volume | affine、mask、units、volume weights、write/round-trip |
| T11 | GIFTI/FreeSurfer surface | geometry vs metric/shape data、coordinate roles、mesh weights |
| T12 | CIFTI grayordinate | structures、brain models、surface/volume components |
| T13 | ROI × subject/cohort | layer/measure/subject 的正确维度与 group design |
| T20 | topology、distance、weights | 决策表和 disconnected/folded surface 反例 |
| T21 | Moran/LISA/null | null 选择、多重比较、结果解释 |
| T22 | parcellation/change of support | support、守恒、coverage、aggregation semantics |
| T23 | group inference | 独立单位、exchangeability、effect/CI/p 值 |
| T30 | transform-aware resampling | supplied transform、method、QC、nonclaim |
| T31 | support uncertainty | uncertainty target、operator model、coverage |
| T32 | spatial models | 每个模型独立教程，不再在 80 行内混讲所有方法 |
| T33 | scalable/file-backed | 资源预算、chunk、hash、failure recovery |
| T34 | paper reproduction | manifest、replay、source data、one-command rebuild |

每篇教程必须包含：目标读者、先修知识、许可明确的 fixture、预计时间/内存、学习目标、完整执行代码、预期输出、结果解释、常见错误、可/不可声明内容、报告模板和下一篇。

#### 其他顶层内容区

教程之外还必须独立设置：

- **How-to recipes**：读取/写出、选择 layer、绑定 subjects、构建 weights、聚合、显式 transform、BIDS derivatives 与故障排查；
- **API Reference**：由 lifecycle registry 生成，先显示小型 core-stable，再显示 method-stable/experimental；
- **Methods & assumptions**：公式、estimand、null、限制、方法引用与 validation report；
- **Validation & reproducibility**：版本绑定证据、benchmarks、fixtures、manifest/replay；
- **Developer / NGCS spec**：schema、ADR、promotion 与 conformance，不能混入普通教程；
- **Migration / NEWS**：只承载版本变化、弃用和转换路径。

`IA-101` 必须定义每类内容的归属规则，防止内部规范再次回流到用户教程。

---

## 5. 软件工程审计

### 5.1 P0：所谓“原子覆盖”可能丢失旧输出

`R/execution.R:52-88` 的 `.ngeo_atomic_write()` 在 `overwrite = TRUE` 时：

1. 写入同目录临时文件；
2. `unlink(path)` 删除已有目标；
3. `file.rename(temporary, path)`；
4. 如果重命名失败，只报错，没有恢复旧文件。

该 helper 被 manifest、replay、resampling、schema、file-backed 输出等多处调用。它对首次写入较安全，但覆盖流程不具备失败原子性。

**修复要求**：优先使用平台提供的 atomic replace；若无法跨平台保证 crash atomicity，应把承诺准确改为 failure-safe overwrite。对 temporary write、backup rename、final replace、rollback、cleanup 与进程中断恢复逐个做 fault injection，证明旧文件可恢复且下次调用不会被残留状态破坏。

### 5.2 P1：公开 API 契约没有覆盖公开面

`inst/spec/API-6.0.md` 把构造器、访问器、I/O、空间操作和未标 experimental 的分析函数描述为 stable，但 `tools/run-contract-60-audit.R` 只冻结 7 个 multilayer 入口，并断言 `stable_count == 7`。CI 名称却是 “Stable 6.0 API contract”。

因此当前存在两个事实来源：

- 规范意义上的广泛 stable API；
- CI 实际保护的 7 个函数。

226 exports 对单维护者过宽，而且还另有 95 个注册 S3 methods 与相关 public generics/classes；直接拆包或立即删除会造成更大破坏。推荐先建立机器可读 lifecycle registry，把每个公开符号分类为：

```text
core-stable
method-stable
experimental
deprecated
developer/exchange
```

然后用一整个 deprecation 周期收敛公开面。没有 registry 和 ADR 前不得增加新的 export。

### 5.3 P1：resource budget 的公开承诺未实现

`ngeo_resource_budget()` 声明 `elapsed_seconds` 是最大运行时间，但包内没有读取或断言它；`blocks` 也只在少数路径生效。四个默认值均为 `Inf`，而许多入口默认使用这个对象，所以“bounded execution”默认并不 bounded。

需要二选一并通过 ADR 固定：

- 实现 block-boundary deadline、取消/清理与可测试的资源中止；或
- 删除/弃用尚未实现的字段，并把默认对象明确命名为 unlimited policy。

不得继续保留文档承诺但运行时无效的字段。

### 5.4 P0：release evidence 没有绑定候选源码

当前 `run-release-evidence-60.R` 对部分输入主要检查 `pass` 字段，没有统一验证 package version、Git SHA、source tarball SHA 和 dependency lock。现有 release/check-output 中仍混有 4.x/5.0 报告，且缺少最终 `validation-suite-60.json` 与 `release-evidence-60.json`。

这使“当前候选通过验证”和“某个历史实现通过验证”无法严格区分。所有科学与性能报告必须绑定：

- package version；
- Git commit；
- source tarball SHA-256；
- R/依赖版本与平台；
- seed、参数、fixture hashes；
- 生成时间与 report schema；
- claim boundary。

聚合器必须拒绝 stale、missing 或版本不一致的证据。

### 5.5 P1/P2：兼容性与性能门禁不足

- DESCRIPTION 声明 R ≥ 4.2，但 CI 没有 R 4.2 + minimum dependency job；
- 7 个大规模用例不在普通 CI 中运行；
- 现有 full-performance 证据早于 6.0 核心重构；
- coverage 报告仍是 4.4.2/5.0 时期，没有 current 6.0 artifact；
- 6.0 明确拒绝读取 5.x serialized objects，却没有可审计迁移工具。

快速 PR CI 可以保持轻量，但至少应增加：

- R 4.2 / minimum dependencies；
- Imports-only clean install；
- PR 级小规模性能 smoke；
- 每周与 release-candidate full performance；
- 5.0 golden objects → 6.x migration/structured failure；
- current coverage artifact。

### 5.6 技术债与过度设计

#### 复杂 orchestration 函数

`ngeo_layer_coupling()`、`plot.ngeo_cortical_map()`、`ngeo_support_uncertainty()`、`ngeo_cortical_map()` 均为数百行、分支很多的 orchestration 函数。可以在 characterization tests 保护下提取阶段性纯 helper，但不应借机重写 public API 或相邻逻辑。

#### 验证基础设施重复

`tools/` 有约 47 个 R/PowerShell 脚本、超过一万行；CI、本地 suite 和 release aggregator 分别硬编码脚本列表，已经发生证据版本不一致。应建立一个 manifest 作为 suite、输出、门禁、超时和证据类型的唯一来源。

#### condition 与 options 过细、分散

源码出现约 111 个 `ngeo_error_*` 子类，部分只出现一次且没有测试；约 28 个 `neurogeo.*` option 分散在算法文件中。应保留稳定 parent conditions，但给公开错误增加统一 `code/field/hint`，并生成 options 参考页。不要为了“整洁”一次性删除所有细分错误类。

#### 历史文件与双重规范

- 11 个活跃 R 文件仍带 `-23/-45/-46/-47/-49` 版本后缀；
- `design/` 与 `inst/spec/` 有 69 个同名文件，只有 21 个字节一致；
- canonical 虽声明为 `inst/spec/`，历史设计仍容易被误改；
- `permute` 在 Suggests 中未发现实际调用；
- `neurogeo5.1_refactor_plan_zh.md` 被打入约 9.6 MB 的 6.0 source tarball。

这些是 P2/P3 清理项，不应先于科学与用户 P0 修复。

### 5.7 治理与供应链

当前治理文件允许直接提交 `main`，只有维护者要求时才使用 PR；没有 tag/GitHub Release，registry submission 需要另行授权。结合单一贡献者、15 天内多次 major/minor 版本跃迁，这不适合可引用的科学软件 release。

建议在正式外部发布前建立：

- protected main 与 required PR review；
- scientific change 至少一名独立方法学 reviewer；
- CODEOWNERS、SECURITY.md、issue/decision templates；
- Actions 固定到 commit SHA；
- SemVer/lifecycle policy、signed tag、GitHub Release；
- CITATION.cff、codemeta、ORCID、Zenodo DOI；
- CRAN/Bioconductor/其他分发渠道的明确决策。

### 5.8 P1：provenance 中的路径与 checksum 策略不一致

`R/io.R:56-74` 的通用 source record 默认保存规范化绝对路径、文件大小、MD5 和读取时间；file-backed identity 的其他路径则使用 SHA-256。绝对路径可能包含受试者、项目或机构目录信息，也使 history 在不同机器间不可移植；MD5 与项目其他完整性证据不一致。

推荐建立显式 source identity policy：

- 默认记录 SHA-256、大小、reader、格式与可选 BIDS URI/相对路径；
- 绝对本地路径默认 redacted，不进入可共享 manifest；
- 用户可显式选择保存原始 URI，但必须有隐私警告；
- replay 使用 logical source ID + hash，不依赖某台机器的绝对路径；
- 旧 MD5 字段按 lifecycle 迁移，不静默改变已有 serialized history。

---

## 6. 科学实现审计

### 6.1 P0：support-weighted regression 实际未使用权重

`R/support-inference.R:24-48` 调用：

```r
stats::lm(outcome ~ predictor, data = frame, spatial_weights = support)
```

R 的参数名是 `weights`。当前 `spatial_weights` 被 `lm.fit()` 忽略并产生 warning。本轮不等权探针显示包结果与 unweighted OLS 完全一致，而不是 weighted reference。Git blame 还表明这一行在 6.0 重命名期间由正确的领域无关词被机械替换。

影响至少包括：

- `ngeo_atlas_robust_effect()`；
- slope 型 `ngeo_support_test()`；
- `ngeo_common_support_test()`；
- `ngeo_boundary_test()` 的相关路径。

这是必须先加失败回归测试、再做一行级修复的典型 surgical task。

### 6.2 P0：surface spin null 有偏且默认边界不安全

当前 `.ngeo_rotation_matrix()` 对 3×3 Gaussian matrix 做 QR，只调整 determinant，没有对 QR 的对角符号做 Haar measure 所需校正。本轮定向探针中旋转矩阵元素经验均值显著偏离 Haar-uniform 所需矩条件；具体 probe 必须由 `EVID-001` 固化后才能作为 release evidence。

此外：

- `strata = NULL` 时所有元素归为 `"all"`；
- 对双半球相同球面坐标，默认最近邻可以跨半球；
- nearest-neighbor mapping 不保证一一对应，会出现 collision/重复目标；
- 输出没有 collision、coverage、跨 structure 诊断。

推荐用随机 quaternion 或经验证的 sign-corrected QR；对于有 structure/hemisphere metadata 的 surface，默认必须保持结构边界。若无法安全推断 strata，则要求显式输入，而不是静默混合。

### 6.3 P0：当前 Moran spectral null 不满足公开声明

代码对对称化图算子做 dense eigendecomposition，再随机翻转全部 eigen coefficients。对于不规则或 row-standardized 图，常数向量通常不是该算子的特征向量；翻转后重新加原均值并不能保证模拟均值不变。

本轮不规则 5 节点图探针中，模拟同时改变了原均值、中心平方和与 Moran's I。本文不把会话内的具体数值当成 release evidence；`EVID-001` 必须先把输入图、values、seed、版本、期望输出和 hash 固化为可重复反例。

因此当前“保持 centered sum of squares 和 Moran quadratic form”的文档声明不成立。可选路径只有两种：

1. 实现并验证正式 Moran spectral randomization 算法及其适用条件；
2. 将当前方法准确命名为受限的 eigen-sign surrogate，删除不成立的 invariant 主张，并限制其稳定性级别。

### 6.4 P0：metric eligibility 与默认 metric 不一致

surface constructor 可标记 coordinate set 为 `metric_eligible = FALSE`，`ngeo_vertex_area()` 会检查；但 `.ngeo_element_coordinates()` 与 edge weights 直接使用 active coordinates。结果是 registration sphere 或 visualization chart 可能被 edge-geodesic/Euclidean 距离路径使用。

同时：

- `ngeo_distance()` 对 surface 默认 edge-geodesic；
- `ngeo_spatial_weights()` 对非 contiguity surface 默认 Euclidean；
- folded/disconnected surface 的 Euclidean KNN 可跨 topology component；
- inverse-distance 对重合点直接求倒数，可产生 `Inf/NaN` 而对象仍构造成功。

需要统一 metric policy：所有 metric operation 先验证 coordinate role/eligibility；surface 非拓扑 weights 要么默认 edge-geodesic，要么强制用户显式选择；跨 component 和零距离必须有明确策略。

### 6.5 P0：graph-distance kriging 可能使用非 PSD covariance

当前 kriging 允许 surface edge-geodesic，把任意 graph shortest-path distance 直接代入 spherical/exponential/Gaussian variogram covariance。一般 graph metric 并不保证由这些 kernel 构成正半定 covariance。代码只在 `solve()` 失败时报 singular，并把负预测 variance 用 `max(0, variance)` 静默截成 0。

本轮会话内探针已构造出合法 surface 上具有负 covariance eigenvalue 的 Gaussian graph-distance 例子，并得到异常预测和被截为 0 的 variance；在 `EVID-001` 固化坐标、faces、kernel 参数、期望结果和版本前，它只能作为高优先级复现线索，不能作为正式 release evidence。

整改应包括：

- 明确哪些 metric × covariance family 有理论保证；
- 对局部 covariance 做 PSD/condition diagnostics；
- 明确 jitter 的用途和上限；
- 不得用截零隐藏实质负 variance；
- 区分 latent process 与 observation variance；
- 无理论支持的组合应拒绝或降为 experimental。

### 6.6 P1：operator ensemble 权重与区间不一致

`ngeo_support_ensemble` 接受非均匀权重，但 Monte Carlo propagation 按 layer 顺序等频循环，没有按 ensemble probability 抽样。另一路 sensitivity summary 用加权 mean/variance，却用未加权 quantile 计算区间。同一结果中的中心与区间不是同一个 estimand。

### 6.7 P1：跨 atlas meta-analysis 存在伪重复风险

同一 source map 经多个 atlas 得到的 effect 高度相关。当前把这些估计送入面向独立研究的 DerSimonian–Laird random-effects formula，不能把 atlas 当作新的独立样本量。即使结果注明 “not parcellation invariance”，SE、Q、I²、tau² 和 pooled p 值仍需要建模相关性。

默认结果应先提供描述性 median/range/sensitivity；若做 inferential consensus，应从 subject/source resampling 估计 atlas covariance，再用 GLS 或层级模型。

### 6.8 P1：部分 bootstrap/permutation 与空间采样机制不匹配

已发现：

- 对 source elements 做 iid bootstrap；
- 重抽 values rows 再放回固定空间位置，破坏 location-value 对应；
- 默认 shuffle predictor 作为 null；
- GWR CV 按 row order 取模分折，结果依赖输入顺序并可能空间泄漏。

这些操作只有在非常具体的 sampling unit/null 下成立。API 必须显式区分：

```text
subject
site
spatial_block
map_null
operator/support
```

错误单位不能只是教程里的 nonclaim，而应在对象与结果 schema 中可检查。

### 6.9 P1：operator-entry independence 不是通用不确定性模型

概率 membership 每一列通常位于 simplex，entries 必然相关。当前 independent Gaussian、截零、再归一化可作为近似，但不能默认代表 calibrated operator uncertainty。应增加 Dirichlet、logistic-normal、empirical registration ensemble 或 block covariance，并通过已知生成模型评估 bias、RMSE 和 interval coverage。

### 6.10 P1：回归结果边界需要收窄

- `ngeo_spatial_lm()` 在 residual 可能空间相关时仍输出普通 iid t 检验，只附 residual Moran；
- GWR 需要 spatial-block CV 与 row-permutation invariance；
- 当前 `ngeo_car()` 更接近 GCV 选择的 penalized smoother，不应被用户理解成完整 Bayesian/Gaussian CAR inference；
- exact/iterative 两套模型族需要统一 estimand/output contract，但应先用 characterization tests 保护数值行为。

### 6.11 需要独立验证的重采样与 null 风险

以下问题当前应列为“待验证”，而不是直接断言错误：

- `ngeo_surface_barycentric_map()` 是把每个 source vertex 投影到 target triangle，再向 target scatter 一个 operator column；这更像 conservative barycentric remapping，不是常见的 target-gather interpolation。API/文档称其为 interpolation，语义需要澄清；
- 大 mesh 只检查 centroid-nearest 的默认 16 个 candidate faces，可能漏掉真实最近 triangle；
- Workbench 对 continuous surface 常推荐 area-aware barycentric resampling，当前没有等价的 area-aware 方法；
- spin/MSR 在 medial wall、不同密度、缺失与趋势下的 type-I/power；
- edge shortest-path 与 continuous-surface geodesic 的差异；
- family/twin/repeated-measures exchangeability 的边界；
- 大于 2,000 元素的 spectral null 与大于 500 元素 experimental MGWR 的可扩展路径。

这些必须通过第 8 节对应 validation task 决定是否实现、改名、限制或删除。

### 6.12 当前科学证据的正确边界

现有 validation suite 是很好的 regression 起点，但不能当成 publication-grade validation：

- Moran calibration 使用 80 个 fields 与 199 permutations；
- SAR/SEM、kriging、GWR 分别约 50、120、60 个 fields；
- 某些低重复 type-I gate 使用较宽的容许区间；
- reference 比较主要是小网格、matched estimand；
- `real-multilayer-50-validation.json` 只有 10 control + 10 epilepsy，并明确写明“not a powered clinical claim”；
- `real-data-422-validation.json` 也明确限制为格式、对齐、资源和 change-of-support validation，而非 clinical validation；
- `inst/spec/validation-6.0.md` 允许沿用历史算法证据，无法捕捉 6.0 机械重命名新引入的 weighted-regression 缺陷。

应保留这些诚实的 claim boundaries，并将 6.x validation 提升为 current-candidate、预注册、高重复、独立 reference 和外部真实数据五个层级。

---

## 7. 发表与领域贡献审计

### 7.1 当前论文是发布阻断项

`paper/paper.md`：

- front matter 仍是 `2.0 preprint draft`；
- 正文仍使用 NGCS 2.0、domain/maps/provenance；
- 声称 “parcellation-invariant inference”，与当前项目自己的 claim boundary 冲突；
- limitation 声称不估计 bandwidth、不做 resampling、不实现 kriging/SAR/CAR，而 6.0 已实现；
- 没有完整作者、机构、参考文献、正式 Methods/Results、figure/source data、Code/Data availability。

`inst/CITATION` 也只有单一 Manual 引用，没有正式姓名/ORCID/DOI。当前论文不应做增量修补，应在科学主张冻结后按 6.x 从头重写；旧草稿可移动到 archive 并明确标记 superseded。

### 7.2 “统一工具箱”不是足够的新颖性

现有生态已经覆盖：

| 工具 | 已有强项 | neurogeo 不应重复宣称 |
|---|---|---|
| [Workbench](https://www.humanconnectome.org/software/connectome-workbench) / FreeSurfer | 标准 surface/CIFTI 操作、重采样、parcellation | 不应把格式操作或 barycentric 本身当原创贡献 |
| [ciftiTools](https://pmc.ncbi.nlm.nih.gov/articles/PMC9119143/) | R 中 CIFTI 读取、写出、操作、可视化 | “R 能处理 CIFTI”不构成新颖性 |
| [neuromaps](https://www.nature.com/articles/s41592-022-01625-w) | 多空间转换、curated annotations、spatial null | “跨空间 brain maps + null”已经有高水平实现 |
| [BrainSpace](https://www.nature.com/articles/s42003-020-0794-7) / [BrainSMASH](https://pubmed.ncbi.nlm.nih.gov/32585343/) | gradients 与 spatial-autocorrelation-aware null | 必须做数值和 type-I 对照，而不是只列功能 |
| [BrainStat](https://pmc.ncbi.nlm.nih.gov/articles/PMC10715099/) / [ENIGMA Toolbox](https://www.nature.com/articles/s41592-021-01186-4) | 统计、上下文化、大规模 consortium 工作流 | 用户生态和真实科学展示门槛很高 |
| spdep/spatialreg/gstat/GWmodel | 成熟空间统计算法 | 经典方法重写必须作为 reference-validated backend，而非主要创新 |

因此不能用“226 个 exports”“支持很多算法”作为论文中心贡献。函数越多，反而越扩大审稿人可攻击的验证面。

### 7.3 推荐的唯一旗舰主张

推荐将论文的**待检验假设**冻结为：

> **检验显式建模 measurement support、跨 atlas/resolution operator 和 operator uncertainty，是否以及在什么边界内能够量化/减少 support-induced bias，并产生校准的跨 support 不确定性。**

该主张需要至少回答四个可证伪问题：

1. 传统 nearest/unweighted/single-atlas 分析会在什么条件下改变 effect、ranking 或 significance？
2. 显式 support semantics 与守恒约束能减少多少 bias？
3. operator uncertainty 的 interval 是否校准，而不只是变宽？
4. 改进能否在独立 cohort、modality、atlas/density/space 中复制？

NGCS、格式互操作、manifest、replay 和资源 guard 是支持该主张的基础设施，不应与中心科学贡献争夺叙事。

### 7.4 NC 路线是条件目标

[Nature Communications 的编辑流程](https://www.nature.com/ncomms/submit/editorial-process)关注 novelty、potential impact、methodological advance 与广泛读者价值，而不是仅有功能丰富的软件。[Nature computational-tools reporting guidance](https://www.nature.com/documents/Computational_tools_reporting_guidelines.pdf)还强调 analytical/reference comparison、安装与测试文档、代表性数据、独立外部验证和可用代码。

因此：

- 如果项目只完成 P0 修复、教程和标准软件发布，它会成为更可靠的领域软件，但不自动达到 NC；
- 如果待检验方法在预注册模拟和项目内部要求的独立真实数据上表现出校准、稳健性与可复制边界，且被外部团队复现，才值得进入 NC go/no-go；阴性结果必须保留，不能以是否产生显著差异决定成功；
- 若 broad-impact gate 未通过，应诚实选择领域方法/软件期刊，而不是继续增加模块制造“看起来更大”的贡献。

### 7.5 发表所需的证据矩阵

| 主张 | 最低证据 | 关键指标 | 反例/消融 |
|---|---|---|---|
| 对齐与空间身份防止静默错配 | adversarial fixtures + user error study | 检出率、误用率、错误可恢复性 | 关闭 metadata guard |
| support aggregation 守恒 | 解析证明 + numeric fixtures | mass error、intensive bias | unweighted、nearest、错误 semantics |
| support uncertainty 校准 | 预注册生成模型 | bias、RMSE、coverage、interval width | independent Gaussian vs simplex/empirical ensemble |
| 跨 atlas inference 有效 | correlated-atlas simulations | type-I、FWER、power、coverage | 独立 meta-analysis、single atlas |
| surface resampling 正确 | analytical phantoms + Workbench reference | interpolation/remap error、coverage、area conservation | nearest、current barycentric、area-aware |
| spatial null 校准 | factorial simulations + established tools | type-I、power、SA preservation、collision | iid shuffle、biased spin、当前 eigen-sign |
| 可扩展且可复现 | 32k/91k/164k + clean rebuild | runtime、peak memory、hash equality | exact vs iterative、cold vs warm |
| 真实科学价值 | discovery + external replication | effect/CI、replication、atlas stability | naive workflow、各组件消融 |

---

## 8. 分阶段可执行路线图

### 8.0 估算与角色

下列工期是粗略的**净人日**，不包含等待数据授权、外部 reviewer 排期、计算队列和投稿审稿时间。它们用于比较相对规模，不应被解释成固定日历承诺。

建议至少区分以下角色，即使早期由同一人兼任：

- **RM**：R/package maintainer；
- **SM**：spatial statistics / null-model reviewer；
- **NM**：neuroimaging methods reviewer；
- **DX**：documentation / UX owner；
- **RE**：release/reproducibility owner；
- **EV**：未参与实现的 external validator。

科学实现的 author 与 scientific reviewer 不应是同一个人。P0 修复可以并行，但同一文件有重叠时必须由一个 Agent 串行合并。

任务表的“依赖”只允许填写明确任务 ID 或“无”。落地为机器 registry 后，CI 必须验证每个 ID 存在并能拓扑排序，禁止循环依赖；阶段名称、未定义的 fixture 或“证据冻结”不得作为依赖占位符。

### 8.1 Phase 0：发布冻结与确定性缺陷修复

目标：消除已知会给出错误结果、静默选错数据或丢失输出的缺陷，并让 6.0 的第一个用户路径真实可运行。

| ID | P | 任务 | Owner / reviewer | 依赖 | 估算 | 验收标准 |
|---|---|---|---|---|---:|---|
| GOV-000 | P0 | 建立临时 feature freeze 与 P0 issue board | RM / SM | 无 | 0.5–1 | P0 全部有 issue、owner、复现脚本和状态；CI 阻止新增 export；README 明确当前开发状态 |
| EVID-001 | P0 | 固化本轮审计反例 corpus | RM/SM / independent reviewer | GOV-000 | 2–4 | layer、weighted regression、atomic replace、spin、MSR、metric、kriging 各有最小脚本/fixture、seed、旧版本失败输出、commit/package/hash；审计数值可一键复现 |
| DOC-001 | P0 | 建立 6.0 单一用户文档真相与入口 smoke test | DX / RM | 无 | 2–4 | README、中英文 guide、网站首页/concepts/tutorial index 与当前手写入口全部扫描和执行；migration/glossary 外不传播旧 API、5.0/5.1 模型；生成页只来自 clean build |
| TERM-001 | P0 | 建立受控词汇表并修复机械替换污染 | DX / NM | GOV-000 | 2–4 | GIFTI metric/shape、spatial weights、coordinate space、distance 等恢复领域含义；用户文本无 MVP/prototype/promotion gate；denylist/allowlist CI 通过 |
| API-001 | P0 | 消除 layer selector 歧义 | RM / NM | EVID-001 | 1–2 | 重名 name 的单 layer 选择报 structured ambiguity 并列出 IDs；按唯一 ID 正确选择；所有单 layer 分析入口有回归测试 |
| ENG-001 | P0 | 实现真实 atomic replace 或准确的 failure-safe overwrite | RM / RE | EVID-001 | 2–4 | 对 write/backup/final replace/rollback/cleanup/interruption 逐点注入失败；目标存在且旧字节可恢复、残留可由下次调用恢复；Windows/macOS/Linux 均测试 |
| SCI-FIX-001 | P0 | 恢复 support-weighted regression | RM / SM | EVID-001 | 1 | 不等权 fixture 与显式 `lm(weights=)` 的 coefficient、SE、t、p 一致；四个下游 API 均有测试；unexpected warning 令测试失败 |
| SCI-FIX-002 | P0 | 修复 spin rotation、strata 与 collision 语义 | SM / EV | EVID-001、TERM-001 | 4–8 | 预注册基于 MCSE 的同时容差；检验 matrix moments、angle/trace/axis reference distribution、正交/行列式及独立实现；hemisphere coupling、medial wall、collision estimand 明确 |
| SCI-FIX-003 | P0 | 修复或降级 Moran spectral null | SM / EV | EVID-001、TERM-001 | 4–8 | 冻结支持的 W 类、中心化/标准化算子与 trend/isolates；directed W 显式拒绝或记录转换；公开 invariant 有数值容差，否则改名、experimental 且禁用 stable inference |
| SCI-FIX-004 | P0 | 统一 metric eligibility、surface 默认与零距离处理 | RM / NM | EVID-001、TERM-001 | 3–6 | registration/visualization/chart 坐标不能进入 metric；folded/disconnected fixture 不静默跨 component；inverse distance 全 finite 或明确拒绝 |
| SCI-FIX-005 | P0 | 为 kriging 加合法 covariance/variance gate | SM / NM | EVID-001、SCI-FIX-004 | 5–9 | ADR 固定 metric×kernel allowlist、relative PSD tolerance、condition gate、scale-relative jitter 上限与负 variance 行为；反例报错，欧氏 reference 继续匹配 |
| REL-001 | P0 | 建立 candidate evidence schema 与 stale-rejection gate | RE / RM | GOV-000 | 2–3 | report schema 绑定 version/commit/tar SHA/deps/fixture/seed；聚合器对 4.x/5.0、missing、hash mismatch 的负向 fixtures 全部拒绝 |
| REL-002 | P0 | 生成 P0 修复后的最终 attestation | RE / RM | DOC-001、TERM-001、API-001、ENG-001、SCI-FIX-001、SCI-FIX-002、SCI-FIX-003、SCI-FIX-004、SCI-FIX-005、REL-001 | 1–2 | 从同一 clean source candidate 生成完整 unit/check/science/doc suite 和单一 attestation；输入报告全部通过 identity gate |
| PAPER-001 | P0 | 阻止陈旧论文被误认为当前描述 | RM / paper lead | GOV-000 | 0.5–1 | 当前 `paper/paper.md` 加明显 superseded 标记或移入 archive；网站不将其称为 6.0 manuscript；不做内容性增量重写 |

#### Phase 0 exit gate

只有同时满足以下条件才解除 feature freeze：

- 上述每个已复现 P0 都有“先失败、后通过”的最小回归测试；
- 单元测试、R CMD check 和相关 scientific probes 全部通过；
- README/guide 在 clean install 中执行，不只做语法渲染；
- 当前用户入口不再传播 5.0/5.1 模型或已删除 API；历史词汇仅在 migration/glossary 的明确上下文出现；
- 无静默 ambiguous layer selection；
- support regression 与 weighted reference 一致；
- spin/MSR 的实现或声明与验证证据一致；
- 若尚未完成 `VAL-301` 的预注册 type-I/power 校准，spin/MSR 的 p 值/null inference 必须禁用或明确标为 experimental；代数 invariant/reference parity 本身不足以恢复 stable inference；
- metric/kriging 反例不再返回看似合法的结果；
- 原子覆盖失败不损坏已有文件；
- `REL-002` 的最终 release attestation 完整绑定同一个 source candidate；
- P0 修复经过至少一名未写该修复的 reviewer。

预计规模：约 30–57 净人日，可由 3 条无文件冲突的工作流并行；不要用日历周替代 exit gate。

### 8.2 Phase 1：稳定概念、API 与第一学习路径

目标：把 6.0 从“内部实现与版本记录”变成 neuroimaging 用户可理解、可迁移、可定位稳定入口的产品。

| ID | P | 任务 | Owner / reviewer | 依赖 | 估算 | 验收标准 |
|---|---|---|---|---|---:|---|
| API-101 | P1 | 为完整 public surface 建 lifecycle registry | RM / RE | REL-002 | 4–7 | exports、95 个 S3 methods、public generics/classes 与 `NAMESPACE` 集合逐项一致且恰有状态/owner；stable formals、返回 schema、conditions 有 snapshot |
| ADR-101 | P1 | 冻结 lifecycle、distribution 与 migration 决策 | RM / RE/DX | API-101 | 1–2 | ADR 分别决定 6.x/next-major deprecation、安装/分发渠道、5.x migration 支持矩阵；记录后果和版本时间线 |
| API-102 | P1 | 增加一致的 `ngeo_*` 访问器并制定 deprecation | RM / DX | API-101 | 2–4 | 新别名稳定可用；旧入口 6.x 不破坏；冲突与 lifecycle 明确；下一 major 若移除 export，则 attach 不再遮蔽 `utils::history` |
| API-103 | P1 | 为 layer index 提供准确名称 | RM / DX | API-101 | 1–2 | `ngeo_layer_index()` 或同等名称与参数使用 unit/feature 词汇；原 `ngeo_validate_layers()` 有兼容层和迁移示例 |
| API-104 | P1 | 建立 measure ID/name 构造和安全更新 | RM / NM | TERM-001、API-101 | 3–5 | 可显式创建去重 measure；更新后 strict validate 且 history 追加；普通教程不直接修改 `$measures` |
| API-105 | P1 | 提供 5.x → 6.x 可审计迁移工具 | RM / RE | ADR-101、API-104 | 5–10 | 支持矩阵明确；常见 in-memory point/surface/volume/parcellation/grayordinate golden objects 必须成功迁移并核对语义；仅未支持复杂类型允许 structured reconstruction report |
| API-106 | P1 | 收紧 unknown space/unit 契约 | RM / NM | TERM-001 | 2–4 | unknown space 不再默认为已知 mm，或物理 metric 明确要求确认；reader 对可靠 header unit 有 golden tests |
| SCI-101 | P1 | 修复 operator ensemble 非均匀权重 | RM / SM | Phase 0 | 2–4 | `(0.9,0.1)` mixture 的 mean/variance/weighted quantile 与手算一致；MC 选择频率在二项容差内且可复现 |
| SCI-102 | P1 | 统一 scientific result inference contract | SM / NM | API-101 | 3–6 | 每个 stable result 可回答 estimand、sampling unit、null、metric、support、uncertainty target；print/summary/manifest 一致 |
| DOC-101 | P1 | 建立文档唯一 manifest 与 freshness CI | DX / RE | DOC-001、TERM-001 | 3–5 | vignette、手写页面、API 索引、双语 route 共用 manifest；临时 clean tree 执行代码/link/edit-link；部署仅使用本次 artifact；route/content hash 拒绝 stale 产物 |
| IA-101 | P1 | 重建首页、侧栏、任务选择器与 glossary | DX / formative users | DOC-101 | 4–7 | 四类数据入口两次点击内到达正确教程；首页、Start、Tutorial、How-to、Concept、Methods、API、Validation、Developer/spec、Migration/NEWS 归属规则明确 |
| DATA-101 | P1 | 建立可许可、可固定的教学 fixture corpus | NM / RE | TERM-001 | 3–6 | NIfTI、surface、CIFTI、ROI/cohort 各有来源、许可、版本、hash、大小、在线/离线策略、预期结果；CI 校验下载与缓存 |
| TUT-101 | P1 | 中英文 15 分钟真实 quickstart | DX / NM | API-104、DATA-101 | 4–6 | clean R 无伪路径、无核心 `eval=FALSE`、无内部 mutation；两种语言输出一致；5 名 formative 用户的中位完成时间 ≤15 分钟 |
| INSTALL-101 | P1 | 安装、版本固定与 optional backend 矩阵 | DX / RE | ADR-101 | 1–3 | Linux/macOS/Windows 安装命令可复制；每种格式/方法明确 dependency、版本和不可用时的替代路径 |
| ERROR-101 | P1 | 用户可见 condition 文本审计 | DX / RM | TERM-001 | 2–4 | 错误说明对象、失败原因、期望与下一步；snapshot 禁止历史里程碑/Agent 备注；核心错误含 code/field/hint |

#### Phase 1 exit gate

- 所有 exports 的生命周期可机器读取；
- 用户不需要理解 4.x/5.x 版本历史就能完成第一次分析；
- quickstart、layer 选择、measure 更新均无直接内部 mutation；
- 当前用户区不再传播 5.0/5.1 模型或 `map/domain/provenance` 旧心智模型，migration/glossary 中的受控出现除外；
- stable 与 experimental 方法在导航、帮助页和运行时输出中一致；
- 恰有一轮至少 5 名 formative neuroimaging 用户测试：允许记录澄清请求，目的为发现问题而非发布成功率；中位 quickstart 时间 ≤15 分钟，所有失败回流为 issue。

预计规模：约 40–75 净人日。

### 8.3 Phase 2：工程收敛与可重复 release

目标：减少单维护者的认知面，让测试、科学验证、文档和 release 使用同一套事实来源。

| ID | P | 任务 | Owner / reviewer | 依赖 | 估算 | 验收标准 |
|---|---|---|---|---|---:|---|
| ENG-201 | P1 | 统一 validation registry/report schema | RE / RM | REL-001 | 3–5 | 一个 manifest 定义 suite、级别、输入、输出、超时、证据；CI/local/release 共用；不再重复硬编码脚本列表 |
| ENG-202 | P1 | 纠正 resource budget 契约 | RM / RE | API-101 | 3–8 | 实现 elapsed deadline 或弃用字段；默认是否 unlimited 明确；每个预算维度有正反测试和清理语义 |
| ENG-203 | P1 | 增加 minimum-R、dependency 与性能 CI | RE / RM | REL-001、ENG-201 | 2–4 | R 4.2/minimum deps、Imports-only、PR smoke、weekly/release full-perf；7 个大规模测试全部生成 current artifacts |
| DOC-201 | P1 | 重写 registry 中全部 stable API 与 Methods pages | DX / SM/NM | API-101、SCI-102 | 20–40 | 以 lifecycle registry 为分母，core-stable 与 method-stable 100% 有 purpose、when/not、units、assumptions、return schema、example、See Also、reference、validation link |
| TUT-201 | P1 | 四条真实格式端到端教程 | DX / NM | TUT-101、DATA-101 | 10–16 | NIfTI、surface、CIFTI、ROI/cohort 全覆盖 read→inspect→semantics/space→QC→analysis→plot→write/history；canonical code 在 CI 执行 |
| LOC-201 | P1 | 建立核心双语 route 与内容对等 | DX / bilingual NM | IA-101、TUT-201、DOC-201 | 3–6 | Start here、四条格式教程、核心 concepts/methods 1:1；locale switch、缺页、断链由 CI 检查；未翻译高级页明确标记 |
| ENG-204 | P2 | 集中 options 与 condition taxonomy | RM / DX | API-101、ERROR-101 | 3–6 | 生成式 options reference；合法范围统一；公开错误有稳定 parent + code/field/hint；不要求一次删除兼容子类 |
| ENG-205 | P2 | 拆分高复杂 orchestration | RM / domain reviewer | API-101、ENG-201 | 5–10 | 先补 characterization tests，再只提取纯内部阶段 helper；公开签名、结果 schema、数值不变；复杂度不再上升 |
| ENG-206 | P2 | 清理 tarball、依赖与历史规范边界 | RM / RE | DOC-101、ENG-201 | 3–6 | 内部计划不进 tar；`design/archive` 与 canonical `inst/spec` 清晰；未用 Suggests 删除；source/install size 有基线 |
| ENG-207 | P1 | 统一 source identity、checksum 与路径隐私 | RM / RE | API-105、REL-001 | 2–4 | 默认 SHA-256 + logical/relative ID；共享 manifest 不含绝对敏感路径；旧 MD5/history 有迁移；跨机器 replay fixture 通过 |
| SEC-201 | P2 | 供应链与维护治理基线 | RM / RE | 无 | 2–4 | Actions 固定 SHA；SECURITY.md、CODEOWNERS、依赖更新策略、最小权限检查、protected main |
| REL-201 | P2 | 正式不可变 prerelease | RE / RM | REL-002、ADR-101、ENG-201、ENG-203、ENG-206、ENG-207、SEC-201、DOC-201、LOC-201 | 2–4 | signed tag、GitHub Release、source/binary hashes、CITATION.cff、codemeta、Zenodo sandbox/DOI 流程、release notes |

#### Phase 2 exit gate

- clean checkout 一条命令生成 package、网站、validation 和 release attestation；
- 所有证据绑定同一候选源码；
- R 4.2 与当前平台矩阵通过；
- 全量性能门禁使用 6.x 当前实现；
- current coverage 已生成并报告，不把 coverage 当作科学有效性证据；
- source tarball 不包含内部计划或陈旧生成物；
- 核心中英文学习路径、route 与执行代码对等；
- 发生 science/API change 必须经 PR 与独立 review；
- 可用 tag/DOI 精确安装论文使用的软件。

预计规模：约 58–113 净人日。

### 8.4 Phase 3：中心方法的科学验证

目标：不是给每个函数补一个图，而是用预注册设计判断旗舰方法是否成立，并把不能成立的功能降级或移除。

| ID | P | 任务 | Owner / reviewer | 依赖 | 估算 | 验收标准 |
|---|---|---|---|---|---:|---|
| PUB-301 | P1 | 冻结 claim–evidence matrix 与分析计划 | paper lead / EV | SCI-102、REL-002 | 4–8 | 每条主张映射公式、simulation、dataset、figure、workflow、停止规则；阈值表与方案 hash 在分析前冻结 |
| VAL-301 | P1 | null-model factorial benchmark | SM / EV | PUB-301、SCI-FIX-002、SCI-FIX-003 | 12–20 | 分析前以机器表冻结 nominal alpha/coverage、等效界值、MCSE/样本量、CI 方法、cell multiplicity、失败率与停止规则；覆盖 SA、trend、mesh/hemisphere/medial wall/irregular/missing，并与成熟工具比较 |
| VAL-302 | P1 | surface/volume/CIFTI resampling reference | NM / EV | PUB-301、SCI-FIX-004 | 12–20 | analytical phantoms + Workbench/FreeSurfer reference；明确 gather interpolation vs conservative remap；area conservation、coverage、error、candidate miss rate |
| VAL-303 | P1 | sampling unit/null API 与校准 | SM / EV | PUB-301、SCI-102 | 8–14 | subject/site/spatial-block/map-null 分离；错误 unit 被拒绝；模拟 coverage 预设范围；复杂 design 可接受用户 schedule/PALM interop |
| VAL-304 | P1 | covariance-aware cross-atlas inference | SM / EV | PUB-301、VAL-303 | 10–16 | atlas correlation 0.5–0.95 模拟中 type-I/coverage 校准；independence 只显式 opt-in；描述性结果为安全默认 |
| VAL-305 | P1 | simplex-consistent operator uncertainty | SM / NM/EV | PUB-301、SCI-101 | 10–18 | Dirichlet/logistic-normal/empirical ensemble 下 bias、RMSE、95% coverage；与 independent Gaussian 做消融 |
| VAL-306 | P1 | 回归、GWR、SAR/SEM/CAR 科学边界 | SM / EV | PUB-301、SCI-FIX-004、SCI-FIX-005 | 12–20 | 与 reference 比 point estimate、SE、type-I、coverage；spatial-block CV 不依赖 row order；CAR 重新命名或实现与主张一致 |
| VAL-307 | P1 | support builder exactness 与 adversarial geometry | NM / EV | PUB-301、VAL-302 | 6–10 | 大 mesh 随机/困难子集与 exact search 比较；报告默认 16 faces miss rate；必要时使用 AABB/BVH；mask/disconnected/partial coverage |
| VAL-308 | P2 | 规模、数值稳定与 exact/iterative benchmark | RM / RE/EV | PUB-301、ENG-203、VAL-301、VAL-302、VAL-303、VAL-304、VAL-305、VAL-306、VAL-307 | 8–14 | 32k/91k/164k、100k subjects/support；精度、收敛、内存、运行时间、失败率；预定义 budgets 和 regression thresholds |
| UX-301 | P2 | 外部 neuroimaging 用户可用性研究 | DX / EV | LOC-201、stable prerelease | 6–10 | 8–15 名未参与开发者、novice/expert 分层、标准化且无开发者协助；任务成功率 ≥80%，quickstart 中位 ≤15 分钟，错误 layer 选择为 0；报告帮助规则、计时和误用类型 |

#### Phase 3 exit gate

- 所有中心方法拥有独立 reference 或明确证明边界；
- null、cross-atlas 和 uncertainty 的 type-I/coverage 达到预注册阈值；
- 失败场景和不适用条件进入运行时 gate 与用户 Methods 文档；
- 每个 flagship component 有消融；
- 至少一名 external validator 可从 release artifact 重跑关键模拟；
- 未达到 gate 的方法降为 experimental、改名或删除，不以“以后再验证”留在 stable 表面。

预计规模：约 88–150 净人日，外加计算资源时间。

### 8.5 Phase 4：真实数据、独立复现与投稿

目标：在允许阴性结果的前提下，检验方法是否解决了真实且具有广泛意义的 neuroimaging 问题，而不是只在软件内部闭环。

| ID | P | 任务 | Owner / reviewer | 依赖 | 估算 | 验收标准 |
|---|---|---|---|---|---:|---|
| PUB-401 | P1 | verified support/transform/operator registry | NM / EV | VAL-302、VAL-307 | 15–30 | 常见 fsaverage/fsLR、MNI、CIFTI density、主流 atlases；每项有版本、方向、hash、source、合法 metric、QC、DOI；不自行重做 registration |
| PUB-402 | P1 | discovery + external replication 数据研究 | paper lead / EV | PUB-301、VAL-301、VAL-302、VAL-303、VAL-304、VAL-305、VAL-306、VAL-307、VAL-308 | 30–60 | 以项目内部 gate 要求至少两个按预定义规则独立的 cohort/site；预定义 endpoints 与 equivalence/robustness 边界；正/阴性均报告；一个数据集完全公开复现 |
| PUB-403 | P1 | 旗舰消融与 competitor benchmark | SM/NM / EV | PUB-402 | 15–25 | area vs unweighted、metric、single atlas vs ensemble、iid vs spatial null、uncertainty model、guard on/off；与主要成熟工具 matched estimand 比较 |
| PUB-404 | P1 | 未参与开发实验室独立复现 | EV | REL-201、PUB-402、PUB-403 | 10–20 | 外部团队仅按公开材料重建至少一张主图；差异和失败公开记录并在 final release 修复/解释 |
| PUB-405 | P1 | 完整开放科学 release | RE / all leads | REL-201、PUB-401、PUB-402、PUB-403、PUB-404 | 8–15 | DOI 固化 code/data/operators/source data；renv/container；一条命令重建全部图表；许可证、ORCID、author contributions 完整 |
| PUB-406 | P1 | 按 6.x 从头重写 manuscript | paper lead / all | PUB-301、PUB-402、PUB-403、PUB-404、PUB-405 | 20–35 | Abstract/Intro/Results/Discussion/Methods 一致；公式、统计、limitations、Code/Data availability、references、source data 完整；无实现矛盾 |
| PUB-407 | P1 | NC go/no-go 外部评审 | 2–3 位外部专家 | PUB-406 | 3–5 | 预审明确回答 novelty、broad interest、technical validity、independent evidence；不满足即选择更合适领域期刊，不扩展功能拖延 |

预计规模：约 101–190 净人日，不包含数据获取/伦理审批、外部实验室排期和投稿审稿时间。

#### NC go/no-go gate

以下是本项目为了降低过度主张风险而设置的内部 gate，不是 Nature Communications 官方规定的固定队列数或结果方向；官方仍以 novelty、potential impact、methodological advance、读者相关性与证据质量评估。

只有同时满足以下条件才建议按 Nature Communications 组织投稿：

1. 中心主张不是“统一 API”，而是一个可证伪、具有广泛意义的方法推进；
2. 在预注册模拟中，bias、coverage、type-I 等 endpoint 达到事先冻结的 equivalence/superiority/robustness 边界，所有正负场景均报告；
3. 在项目内部要求的至少两个独立 cohort/site 中，预定义 endpoint 的 calibration、robustness 与适用边界可复制；允许无差异或阴性结果；
4. 关键结果经过未参与开发团队复现；
5. 代码、operators、source data、环境和全部 figures 有 DOI 与一键重建；
6. 论文主张与 stable/experimental API、known limitations 完全一致；
7. 外部预审认为影响超出单个软件生态。

若任一核心条件不满足，合理结论是“形成可靠的软件/方法论文并继续积累外部证据”，不是新增更多算法。

---

## 9. 横向依赖与推荐执行顺序

### 9.1 关键路径

```text
GOV-000
  ├─ REL-001 (evidence schema)
  ├─ DOC-001 / TERM-001
  └─ EVID-001
       ├─ API-001
       ├─ ENG-001
       └─ SCI-FIX-001…005
              + DOC-001 + TERM-001 + REL-001
              └─ REL-002 (P0 attestation)
                    ├─ API-101 / ADR-101 / API-102…106
                    ├─ DOC-101 / DATA-101 / TUT-101
                    └─ SCI-102 ─ PUB-301 ─ VAL-301…308
                                       └─ PUB-401…407

ENG-201/203/206/207 + DOC-201 + LOC-201 + SEC-201 + REL-002
  └─ REL-201 (immutable prerelease)
```

### 9.2 可并行工作流

Phase 0 可按文件与 reviewer 分成三条：

- **轨道 A：用户/API** — DOC-001、TERM-001、API-001；
- **轨道 B：工程/release** — REL-001 先冻结 schema，ENG-001/PAPER-001 并行，全部 P0 完成后运行 REL-002；
- **轨道 C：科学修复** — SCI-FIX-001–005，但共享文件时串行。

Phase 1 可并行：API registry/aliases、doc manifest/IA、scientific result contract。Phase 3 的模拟设计必须先由 PUB-301 固定，不能边看结果边改主要 gate。

### 9.3 不建议的顺序

- 不要先拆包；先分类 public API、测量耦合度和用户路径；
- 不要先重写全部 54 篇 vignette；先完成一个 perfect quickstart 和四条格式路径；
- 不要先加第二语言实现；NGCS 还没有独立 adopter 与冻结的 conformance corpus；
- 不要先做更多 MGWR/LMC/ordination；它们会扩大验证面，不能解决中心可信度；
- 不要把 Wordsmith 式论文润色放在科学主张和证据冻结之前；
- 不要通过降低测试容差或删除失败场景来满足 exit gate。

---

## 10. 产品边界、风险登记与决策点

### 10.1 推荐保留的范围

`neurogeo` 应专注于：

- neuroimaging spatial base 与数据对齐；
- 读取/写出后的空间身份与 measurement semantics；
- topology、distance、weights 的显式构造；
- parcellation、support map、change of support；
- support/atlas/operator uncertainty；
- 与中心主张直接相关的 null、group inference 和可复现结果；
- Workbench/FreeSurfer/PALM 等成熟工具的可审计 interop。

### 10.2 明确非目标

至少到中心论文完成前，不应扩展为：

- raw MRI preprocessing；
- registration、segmentation、surface reconstruction 的新实现；
- 通用 connectomics/graph learning/ML 平台；
- 任意地理统计方法的百科全书；
- 用 R 重写 Workbench/FreeSurfer/PALM；
- 未经独立实现采用就自称 community standard；
- 为了“看起来完整”增加更多容器、setter 或 facades。

成熟工具已经可靠解决的部分应通过 adapter、verified command manifest 或 reference comparison 复用。

### 10.3 风险登记表

| 风险 | 可能后果 | 当前信号 | 缓解任务 | 停止条件 |
|---|---|---|---|---|
| 静默选错 layer | 分析错误但无警告 | 已复现 duplicate name | API-001 | 任一 stable single-layer API 仍可静默歧义选择 |
| 科学参数被机械改名 | 错误结果 | support `weights` 已变成无效参数 | TERM-001、SCI-FIX-001 | 仍存在用户/算法词汇无语义替换 |
| null model 未校准 | type-I/p 值错误 | spin、MSR 反例 | SCI-FIX-002、SCI-FIX-003、VAL-301 | 预注册 type-I gate 系统失败 |
| metric/covariance 不合法 | 跨 component、负 variance、异常预测 | 已有代码路径和反例 | SCI-FIX-004/005、VAL-302/306 | 合法输入仍返回无法解释的 finite 结果 |
| atlas 伪重复 | CI 过窄、p 值夸大 | 当前独立 meta-analysis formula | VAL-304 | correlated-atlas 模拟无法校准 |
| operator uncertainty 错配 | coverage 错误 | 非均匀权重与 quantile 不一致 | SCI-101、VAL-305 | 主要生成模型 coverage 未达 gate |
| 文档漂移 | 用户无法判断当前 API | 5.0/5.1/6.0 混杂、首页报错 | DOC-001/101 | clean build 仍产生 stale/断链页面 |
| API 面失控 | 单维护者无法稳定支持 | 226 exports、只有 7 个 contract freeze | API-101 | 新 export 无 lifecycle/owner/reference |
| 输出覆盖失败 | manifest/结果丢失 | `.ngeo_atomic_write()` 已确认 | ENG-001 | fault injection 仍损坏旧文件 |
| 陈旧证据被用于新版本 | 假阳性 release attestation | 6.0 聚合可接受历史报告 | REL-001、REL-002 | 任一证据不能追溯同一 tar SHA |
| provenance 泄露绝对路径 | 隐私与跨机器不可移植 | source record 默认 absolute path + MD5 | ENG-207 | 共享 manifest 仍依赖或暴露本机绝对路径 |
| 单维护者与无 tag | bus factor、不可引用 | direct main、无 release/tag | SEC-201、REL-201 | 论文仍依赖可变 `main` |
| 论文主张过宽 | desk reject / reviewer loss of trust | 2.0 草稿与 6.0 矛盾 | PUB-301、PUB-406 | claim–evidence matrix 不能闭环 |

### 10.4 必须由维护者明确的决策

下列选择不能由 Agent 静默决定，但 Agent 可以准备 ADR 与证据：

1. **旗舰主张**：是否接受 support-aware multiresolution inference 作为唯一主线；
2. **API 生命周期**：下一 major 是否移除无前缀访问器；
3. **5.x migration**：提供 best-effort converter，还是只提供可审计 reconstruction report；
4. **surface metric 默认**：edge-geodesic、安全拒绝，还是要求每次显式选择；
5. **当前 MSR**：实现正式算法还是改名并降级；
6. **CAR 命名**：明确为 penalized smoother，还是实现完整概率模型；
7. **分发渠道**：CRAN、Bioconductor、R-universe 或只做 GitHub/Zenodo；
8. **真实数据研究**：可用 cohort、伦理/许可、discovery/replication 划分；
9. **NC go/no-go**：由外部预审决定，而不是由功能完成度决定。

每项决策应形成短 ADR：context、options、decision、consequences、evidence、reviewer、date。没有 ADR 时保持当前行为并不得扩大主张。

---

## 11. 后续 Agent 执行协议

### 11.1 开始任务前

每个 Agent 必须：

1. 阅读本文、`inst/spec/NGCS-6.0.md`、`inst/spec/API-6.0.md` 和 `inst/spec/migration-6.0.md`；
2. 只领取一个已有任务 ID；如果任务太大，先拆成不重叠的子任务；
3. 记录当前 commit、working tree 状态与相关 baseline 命令；
4. 明确这是 bug fix、contract change、scientific validation、documentation 还是 release task；
5. 对科学任务先写 estimand、sampling unit、null、metric、support 和预期 invariant；
6. 如果需要改变 public API、科学主张或数据格式，先提交 ADR，不得静默选择。

### 11.2 实现规则

- **一个任务一个 PR**；每行变更能追溯到任务验收标准；
- bug fix 必须先加入最小失败测试；
- 不做无关重构、格式化或相邻“顺手改进”；
- 禁止无边界 global search/replace；标识符、自然语言、神经影像专有词分别审查；
- 不新增 export，除非任务明确授权且 lifecycle registry、帮助页、测试、migration 同步；
- 不直接修改用户对象内部表作为正常工作流；
- 不把 warning 静默吞掉；如果 warning 是预期边界，要有 class、test 和文档；
- scientific fix 不得只断言 finite/length/class，必须与手算、解析 invariant 或独立 reference 比较；
- randomized tests 固定 seed，但 calibration 必须跨多 seed/report CI；
- performance fix 同时报告精度、内存、时间和失败语义；
- 生成物不得手工修补，必须从 canonical source 重建。

### 11.3 PR 必须包含的证据

建议 PR 模板：

```text
Task ID:
Problem reproduced by:
Root cause:
Files intentionally changed:
Files intentionally not changed:
Public API impact:
Scientific estimand/claim impact:
Tests before fix:
Tests after fix:
Reference implementation or analytic invariant:
Documentation/migration impact:
Release evidence impact:
Known limitations:
Independent reviewer:
```

### 11.4 科学任务的最低证据层级

| 层级 | 证据 | 可支持的结论 |
|---|---|---|
| L0 | class/finite/smoke test | 只能说明函数运行，不说明数值正确 |
| L1 | 手算小例、代数 invariant | 支持局部实现正确性 |
| L2 | 独立 package/CLI reference | 支持 matched estimand 数值一致 |
| L3 | factorial simulation | 支持预定义场景下的 calibration/power |
| L4 | 真实数据 + sensitivity/ablation | 支持应用价值与限制 |
| L5 | 独立 cohort/lab replication | 支持可推广的论文主张 |

确定性数值原语的 stable claim 至少需要 L1 加独立 oracle/reference；两个实现可能共享偏差，所以 reference parity 不能替代解析检查。任何 p 值、CI、null 或 uncertainty claim 还必须有 L3 calibration/coverage。原创 estimand 没有成熟 package 时，可以用解析证明加未参与开发者的独立重实现代替竞品 parity。NC 中心主张需要 L3+L4+L5。

### 11.5 任务完成定义

Agent 只有在以下全部完成时才能把任务标为 done：

- 验收标准逐项给出可核对结果；
- 新失败测试在旧实现上确实失败；
- targeted tests、full unit suite 和相关 check 通过；
- 文档、examples、migration 和 lifecycle 同步；
- 生成证据带 commit/version/hash；
- 没有 unrelated diff；
- reviewer 与作者不是同一个人（P0 science/release task）；
- known limitations 和没有解决的相邻问题被明确记录；
- 没有通过放宽科学 gate、隐藏 warning 或删除反例来“通过”。

### 11.6 何时停止并请求维护者决策

遇到以下任一情况，Agent 应停止实现并提交 ADR/问题说明：

- 两个合理解释会改变 public behavior 或 serialized format；
- 修复需要破坏 6.x API；
- reference implementation 与当前数学规范不一致；
- 预注册 calibration gate 失败且没有明确 root cause；
- 数据许可不允许公开 fixture/source data；
- 需要新增重量级依赖或外部 binary；
- 任务会扩展到 raw preprocessing/registration/segmentation；
- 同一问题连续三次修复仍出现同类科学反例。

---

## 12. 项目级 Definition of Done

### 12.1 面向用户的稳定版本

- tagged/DOI 版本可以在支持的平台 clean install；
- README 与四条代表性工作流全部执行；
- 用户不用读历史规范即可理解 base/layer/measure/support；
- 用户不能静默选择歧义 layer 或不合法 metric；
- stable/experimental/deprecated 状态处处一致；
- 所有稳定方法帮助页说明适用/不适用场景、单位、假设和结果；
- 先完成 Phase 1 的至少 5 名 formative 测试，再由 `UX-301` 用 8–15 名未参与开发、novice/expert 分层用户做无开发者协助的 release validation；任务成功率至少 80%，quickstart 中位不超过 15 分钟，错误 layer 选择为 0。

### 12.2 软件工程稳定版本

- R CMD check 矩阵、minimum R/deps、current coverage、full performance 均通过；
- release evidence 绑定同一个 source tarball；
- 原子覆盖、source mutation、manifest/replay 有 failure-recovery tests；
- public exports 全部有 lifecycle 与 owner；
- 没有声明但从未实现的 resource fields；
- package/source tarball 不包含内部计划和陈旧生成物；
- protected main、review、security、signed tag 和 DOI 到位。

### 12.3 科学稳定版本

- 每个 stable analysis 都有明确 estimand、sampling unit、null、metric、support；
- matched estimand 与解析/独立 reference 一致；
- randomized inference 的 type-I、coverage、power 有预注册模拟；
- surface/volume/CIFTI reference workflows 有 golden fixtures；
- no silent negative variance、non-finite weights、cross-component metric 或 unsupported covariance；
- atlas/support 不作为伪独立样本量；
- 未达到 gate 的方法明确 experimental 或移除。

### 12.4 论文与 NC 条件

- 一个中心主张、一个 claim–evidence matrix；
- 按项目内部 go/no-go 预先定义至少两个独立 cohort/site 与一个完全公开复现 workflow；这不是 Nature Communications 的固定官方队列数规则；
- 主要组件与成熟 competitor 的 matched-estimand benchmark；
- 关键图由外部实验室复现；
- figures/source data/code/operators/environment 全部 DOI 固化；
- manuscript 与 6.x 实现、限制、API status 一致；
- 外部预审支持 broad-interest 定位。

---

## 13. 参考依据与外部定位

### 13.1 Nature / reporting guidance

- [Nature Communications editorial process](https://www.nature.com/ncomms/submit/editorial-process)：编辑评估 novelty、potential impact、methodological advance 与读者相关性；
- [Nature Communications peer-review criteria](https://www.nature.com/ncomms/editorial-policies/peer-review)：评审关注证据质量、技术有效性和结论支持程度；
- [Nature Communications guide to authors](https://www.nature.com/ncomms/submit/guide-to-authors)：强调重要推进与广泛读者价值；
- [Nature Communications how to submit](https://www.nature.com/ncomms/submit/how-to-submit)：Methods、Data/Code availability、source data 与可重复性要求；
- [Nature computational tools reporting guidelines](https://www.nature.com/documents/Computational_tools_reporting_guidelines.pdf)：reference/analytical validation、安装文档、测试、examples 与外部验证；
- [Nature statistical guidance](https://www.nature.com/documents/ncomms_-_statisticalguidance.pdf)：样本量、检验、effect size、CI、实际 p 值与 null 解释；
- [Nature Communications manuscript checklist](https://www.nature.com/documents/ncomms-manuscript-checklist.pdf)：Methods、availability、source data、author contributions 等提交检查。

### 13.2 领域工具与方法参照

- [neuromaps, Nature Methods](https://www.nature.com/articles/s41592-022-01625-w)；
- [BrainSpace, Communications Biology](https://www.nature.com/articles/s42003-020-0794-7)；
- [ENIGMA Toolbox, Nature Methods](https://www.nature.com/articles/s41592-021-01186-4)；
- [BrainStat](https://pmc.ncbi.nlm.nih.gov/articles/PMC10715099/)；
- [ciftiTools](https://pmc.ncbi.nlm.nih.gov/articles/PMC9119143/)；
- [BrainSMASH](https://pubmed.ncbi.nlm.nih.gov/32585343/)；
- [Alexander-Bloch et al. spin tests](https://pmc.ncbi.nlm.nih.gov/articles/PMC6095687/)；
- [Wagner & Dray Moran spectral randomization](https://doi.org/10.1111/2041-210X.12407)；
- [Connectome Workbench CIFTI resampling](https://www.humanconnectome.org/software/workbench-command/-cifti-resample)；
- [Nilearn documentation](https://nilearn.github.io/stable/index.html)；
- [Neuroconductor](https://pmc.ncbi.nlm.nih.gov/articles/PMC6409417/)。

这些参照的用途是定义 matched estimand、竞争基线和用户预期，不是要求 `neurogeo` 复制它们的全部功能。

---

## 14. 最终建议

### 14.1 接下来立即做什么

第一批只启动以下工作包：

1. `GOV-000 + EVID-001 + REL-001`：冻结功能，固化反例，并先建立候选证据 schema；
2. `SCI-FIX-001`：恢复 support regression 权重并加 reference test；
3. `API-001`：消除 duplicate layer name 的静默选择；
4. `ENG-001`：实现可验证的 atomic replace 或 failure-safe overwrite；
5. `DOC-001 + TERM-001`：让全部当前用户入口使用同一 6.0 真相，并清理危险术语污染；
6. `SCI-FIX-002 + SCI-FIX-003`：由独立方法 reviewer 修复或降级 spin/MSR；
7. `SCI-FIX-004 + SCI-FIX-005`：收紧 metric 与 kriging 合法组合；
8. `REL-002`：用同一 clean candidate 关闭 Phase 0 evidence gate。

Phase 0 内先完成 `REL-001` 的 evidence schema，再以 `REL-002` 关闭 gate；随后启动 API lifecycle、真实 quickstart 与中心 claim–evidence matrix。

### 14.2 不应做什么

当前不应：

- 发布“6.0 已科学验证”的公告；
- 基于现有 spin/MSR/support regression 生成新的论文结果；
- 添加新的 public spatial models；
- 把 54 篇教程逐篇做表面润色；
- 把 NGCS 称为已形成独立生态的标准；
- 以函数数量或“支持所有方法”作为 NC 叙事。

### 14.3 成功后的产品形态

一个成功的 `neurogeo` 不需要成为最大的 neuroimaging 包。它应当让用户清楚、可靠地回答：

```text
我的值究竟定义在哪个空间与 support 上？
这些 layers 的物理/统计含义是什么？
跨 atlas、density 或 modality 的映射是否守恒、覆盖是否完整？
不确定性来自 subjects、sites、spatial null，还是 support/operator？
结论是否依赖某个 atlas 或不合法的 metric/null？
别人能否用同一个 release、operator 和 manifest 重建结果？
```

如果项目能以一条短而完整的路径回答这些问题，并用独立证据估计 support-induced bias 是否存在、能否减少以及适用边界，它就可能形成真正有辨识度的领域贡献；高质量阴性结果也应改变主张而不是被隐藏。若证据不支持中心假设，继续扩展功能只会让维护与审稿风险更高。
