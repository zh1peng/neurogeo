---
title: 安装、版本固定与可选后端
description: 在 Linux、macOS 和 Windows 安装 neurogeo 6.0，并按任务选择依赖
---

# 安装、版本固定与可选后端

## 先选择版本承诺

仓库目前**没有 6.0 release tag**。因此，`main` 是会变化的开发分支，不是可复现的
稳定发布。正式发布前，不会把尚不存在的 `v6.0.0` 命令描述为可用。

- 要评估当前源码：从本地 checkout 构建并安装；
- 要跟随开发：显式安装 `main`，并记录安装时的 40 位 commit SHA；
- 要做可发表分析：等待 `v6.0.0` tag、release tarball 和相应验证证据，或由研究团队固定并归档一个已审核 commit。

## 三个平台的可复制命令

所有平台均要求 R 4.2.0 或更新版本以及能够编译 R source package 的工具链。

Linux 或 macOS，在仓库根目录运行：

```sh
R CMD build .
R CMD INSTALL neurogeo_6.0.0.tar.gz
```

Windows PowerShell，在仓库根目录运行：

```powershell
R.exe CMD build .
R.exe CMD INSTALL neurogeo_6.0.0.tar.gz
```

如果 `R.exe` 不在 `PATH`，请使用 R 安装目录下 `bin\R.exe` 的完整路径。PowerShell
中的 `R` 可能是 `Invoke-History` 的别名，因此这里有意写成 `R.exe`。

只用于开发评估的可变安装：

```r
install.packages("remotes")
remotes::install_github("zh1peng/neurogeo@main")
```

安装后记录实际版本和来源：

```r
packageVersion("neurogeo")
utils::sessionInfo()
```

正式 tag 存在后，稳定安装命令将是
`remotes::install_github("zh1peng/neurogeo@v6.0.0")`。在 release 页面出现该 tag
之前，请不要运行或引用它作为已有发布。

## 文件格式后端

下表中的“6.0 审核版本”是本次兼容性环境中的版本，不是隐含的最低版本；
`DESCRIPTION` 尚未声明这些 Suggests 的最低版本。发布前应由锁定环境和多平台 CI
确定最低支持版本。

| 任务 | 可选 dependency | 6.0 审核版本 | 未安装时的替代路径 |
|---|---|---:|---|
| NIfTI 读取、写出、file-backed 读取 | `RNifti` | 1.9.0 | 用 `ngeo_volume()` 从已读入的 array、affine 和 mask 构造；不能直接读写 NIfTI |
| GIFTI 读取 | `gifti` | 0.9.0 | 用 `ngeo_surface()` 从 coordinates、faces 和 values 构造 |
| GIFTI 写出 | `freesurferformats` | 1.0.1 | 保留 `ngeo_surface` 对象，或导出普通 matrix/data frame；不能直接写 GIFTI |
| CIFTI 读取 | `cifti` | 0.5.0 | 用 `ngeo_grayordinates()` 从 brain model、values 和可选 surface 构造 |
| CIFTI file-backed metadata | `xml2` | 1.5.1 | 使用完整读取，或先在外部提取元数据；不能使用 file-backed CIFTI 路线 |
| CIFTI dscalar/dlabel/dtseries 写出 | 包内 pure-R writer | 6.0.0 | 无额外格式后端；输入仍须通过显式 CIFTI contract 校验 |
| FreeSurfer surface/annot/curv/MGH/MGZ 读写 | `freesurferformats` | 1.0.1 | 用相应 native constructor；不能直接读写 FreeSurfer 文件 |
| BIDS JSON、support bundle/manifest | `jsonlite`（Import） | 2.0.0 | 核心安装已包含；若被移除，相应 JSON 交换功能不可用 |

这些 reader/writer 都不需要 FreeSurfer、FSL 或 Connectome Workbench 可执行文件。

## 方法后端

| 任务 | dependency | 6.0 审核版本 | 未安装时的行为或替代路径 |
|---|---|---:|---|
| 大规模 kNN、distance band、support matching；实验 surface spin matching | `dbscan` | 1.2.4 | 小数据可选择不触发 scalable backend 的 base 路线；surface spin 不可用 |
| `igraph` 转换 | `igraph` | 2.3.0 | 保留 `ngeo_spatial_weights` 或 sparse matrix 表示 |
| `spdep` 转换；实验 spatial ordination | `spdep` | 1.4.2 | 保留包内 weights；ordination 不可用 |
| 大型 spatial basis eigensolver | `RSpectra` | 本环境未安装 | 小问题使用包内 dense fallback；超出保护阈值时缩小问题或安装后端 |
| `sf` 互操作 | `sf` | 1.1.0 | 使用 native base geometry 与 data frame |
| 实验 coregionalization | `gstat` + `sf` | 2.1.6 + 1.1.0 | 不提供自动替代；方法不可用 |
| 实验 MGWR | `GWmodel` + `sf` | 2.4.1 + 1.1.0 | 使用 stable 单带宽 GWR，或明确安装实验后端 |
| 实验 spatial ordination | `ade4` + `adespatial` + `spdep` | 本环境未安装 | 不提供自动替代；方法不可用 |

`spatialreg`、`permuco`、`permute` 是当前 Suggests，但 6.0 公共运行路径不要求它们；
保留它们不应被理解为用户必须安装。Phase 2 会收敛这些声明。

## 失败时怎么看错误

缺少可选后端会产生 `ngeo_error_backend`，而不是在计算途中静默换算法。错误的
`field` 指向缺失的 package，`hint` 给出安装命令。若研究环境不允许添加依赖，
请使用上表的 native constructor 或保留包内表示，不要把不同后端的结果当作同一算法。

返回[安装与第一次运行](/guide/)或进入[15 分钟快速开始](/tutorials/getting-started)。
