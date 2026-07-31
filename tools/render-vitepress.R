#!/usr/bin/env Rscript

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("Rendering the tutorial site requires the `rmarkdown` package.")
}
if (!requireNamespace("knitr", quietly = TRUE)) {
  stop("Rendering the tutorial site requires the `knitr` package.")
}
if (!requireNamespace("neurogeo", quietly = TRUE)) {
  stop("Install neurogeo before rendering the tutorial site.")
}

flatmap_figures <- c(
  "conte69-vertex-flatmap.png",
  "conte69-atlas-flatmap.png"
)
flatmap_sources <- file.path("vignettes", "figures", flatmap_figures)
if (all(file.exists(flatmap_sources))) {
  flatmap_target <- file.path("website", "public", "images")
  dir.create(flatmap_target, recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(
    flatmap_sources,
    file.path(flatmap_target, flatmap_figures),
    overwrite = TRUE
  )
  if (!all(copied)) {
    stop("Could not copy cortical flatmap figures into the website.")
  }
}

pairs <- data.frame(
  zh_source = c(
    "vignettes/core-concepts-zh.Rmd",
    "vignettes/reading-data-zh.Rmd",
    "vignettes/schema-validation-zh.Rmd",
    "vignettes/interoperability-zh.Rmd",
    "vignettes/scalable-io-zh.Rmd",
    "vignettes/file-backed-io-zh.Rmd",
    "vignettes/neighbors-and-weights-zh.Rmd",
    "vignettes/parcellation-and-aggregation-zh.Rmd",
    "vignettes/change-of-support-zh.Rmd",
    "vignettes/real-world-support-mapping-zh.Rmd",
    "vignettes/transform-aware-resampling-zh.Rmd",
    "vignettes/space-transform-graph-zh.Rmd",
    "vignettes/support-uncertainty-zh.Rmd",
    "vignettes/support-aware-inference-zh.Rmd",
    "vignettes/spatial-modelling-zh.Rmd",
    "vignettes/model-uncertainty-zh.Rmd",
    "vignettes/iterative-spatial-models-zh.Rmd",
    "vignettes/spatiotemporal-analysis-zh.Rmd",
    "vignettes/bounded-execution-zh.Rmd",
    "vignettes/reproducible-replay-zh.Rmd"
  ),
  en_source = c(
    "vignettes/core-concepts.Rmd",
    "vignettes/reading-data.Rmd",
    "vignettes/schema-validation.Rmd",
    "vignettes/interoperability.Rmd",
    "vignettes/scalable-io.Rmd",
    "vignettes/file-backed-io.Rmd",
    "vignettes/neighbors-and-weights.Rmd",
    "vignettes/parcellation-and-aggregation.Rmd",
    "vignettes/change-of-support.Rmd",
    "vignettes/real-world-support-mapping.Rmd",
    "vignettes/transform-aware-resampling.Rmd",
    "vignettes/space-transform-graph.Rmd",
    "vignettes/support-uncertainty.Rmd",
    "vignettes/support-aware-inference.Rmd",
    "vignettes/spatial-modelling.Rmd",
    "vignettes/model-uncertainty.Rmd",
    "vignettes/iterative-spatial-models.Rmd",
    "vignettes/spatiotemporal-analysis.Rmd",
    "vignettes/bounded-execution.Rmd",
    "vignettes/reproducible-replay.Rmd"
  ),
  zh_target = paste0(
    "website/modules/",
    c(
      "core-concepts", "reading-data", "schema-validation",
      "interoperability", "scalable-io", "file-backed-io",
      "neighbors-and-weights", "parcellation-and-aggregation",
      "change-of-support", "real-world-support-mapping",
      "transform-aware-resampling", "space-transform-graph",
      "support-uncertainty", "support-aware-inference",
      "spatial-modelling", "model-uncertainty",
      "iterative-spatial-models", "spatiotemporal-analysis",
      "bounded-execution", "reproducible-replay"
    ),
    ".md"
  ),
  en_target = c(
    "website/en/tutorials/core-concepts.md",
    "website/en/tutorials/reading-data.md",
    "website/en/modules/schema-validation.md",
    "website/en/modules/interoperability.md",
    "website/en/modules/scalable-io.md",
    "website/en/modules/file-backed-io.md",
    "website/en/tutorials/neighbors-and-weights.md",
    "website/en/tutorials/parcellation-and-aggregation.md",
    "website/en/tutorials/change-of-support.md",
    "website/en/modules/real-world-support-mapping.md",
    "website/en/modules/transform-aware-resampling.md",
    "website/en/modules/space-transform-graph.md",
    "website/en/modules/support-uncertainty.md",
    "website/en/modules/support-aware-inference.md",
    "website/en/tutorials/spatial-modelling.md",
    "website/en/modules/model-uncertainty.md",
    "website/en/modules/iterative-spatial-models.md",
    "website/en/modules/spatiotemporal-analysis.md",
    "website/en/modules/bounded-execution.md",
    "website/en/modules/reproducible-replay.md"
  ),
  zh_title = c(
    "核心概念与对象契约",
    "读取神经影像数据",
    "Schema 验证与可移植 manifest",
    "互操作与可审计交换",
    "可扩展 values、CIFTI 与 BIDS derivatives",
    "文件后端神经影像 values",
    "邻接关系与空间权重",
    "分区与聚合",
    "空间支持变换与跨 atlas 分析",
    "真实数据中的 support mapping",
    "显式 transform 的 resampling",
    "显式空间与 transform path",
    "Support uncertainty 与 operator ensemble",
    "Support-aware inference",
    "有界空间建模",
    "包含不确定性的空间模型",
    "有界迭代空间模型",
    "显式时间与时空分析",
    "有界科学计算",
    "可审计 replay 与 derivative artifact"
  ),
  en_title = c(
    "Core concepts",
    "Reading neuroimaging data",
    "Schema validation and portable manifests",
    "Interoperability and auditable exchange",
    "Scalable values, CIFTI, and BIDS derivatives",
    "File-backed neuroimaging values",
    "Neighbors and weights",
    "Parcellation and aggregation",
    "Change of support and cross-atlas analysis",
    "Real-world support mapping",
    "Transform-aware resampling",
    "Explicit spaces and transform paths",
    "Support uncertainty and operator ensembles",
    "Support-aware inference",
    "Bounded spatial modelling",
    "Uncertainty-aware spatial models",
    "Bounded iterative spatial models",
    "Explicit temporal and spatiotemporal analysis",
    "Bounded scientific execution",
    "Auditable replay and derivative artifacts"
  ),
  stringsAsFactors = FALSE
)

documents <- rbind(
  data.frame(
    source = c(
      "vignettes/getting-started-zh.Rmd",
      "vignettes/format-workflows-zh.Rmd"
    ),
    target = c(
      "website/tutorials/getting-started.md",
      "website/tutorials/format-workflows.md"
    ),
    title = c(
      "点数据空间统计：从 ngeo_points 到 Moran's I",
      "标准格式 I/O：NIfTI、GIFTI、CIFTI 与 FreeSurfer"
    ),
    counterpart = NA_character_,
    counterpart_label = NA_character_,
    stringsAsFactors = FALSE
  ),
  data.frame(
    source = pairs$zh_source,
    target = pairs$zh_target,
    title = pairs$zh_title,
    counterpart = sub("^website", "", sub("\\.md$", "", pairs$en_target)),
    counterpart_label = "English",
    stringsAsFactors = FALSE
  ),
  data.frame(
    source = pairs$en_source,
    target = pairs$en_target,
    title = pairs$en_title,
    counterpart = sub("^website", "", sub("\\.md$", "", pairs$zh_target)),
    counterpart_label = "简体中文",
    stringsAsFactors = FALSE
  )
)

documents <- rbind(
  documents,
  data.frame(
    source = "vignettes/interoperability.Rmd",
    target = "website/en/modules/interoperability-29.md",
    title = "Interoperability and auditable exchange",
    counterpart = "/modules/interoperability",
    counterpart_label = "简体中文",
    stringsAsFactors = FALSE
  )
)

documents <- rbind(
  documents,
  data.frame(
    source = c(
      "vignettes/cortical-cartography-zh.Rmd",
      "vignettes/cortical-cartography.Rmd"
    ),
    target = c(
      "website/modules/cortical-cartography.md",
      "website/en/modules/cortical-cartography.md"
    ),
    title = c(
      "真实皮层二维地图：vertex 数据、沟回底图与任意 atlas",
      "Real cortical flatmaps for vertex data and arbitrary atlases"
    ),
    counterpart = c(
      "/en/modules/cortical-cartography",
      "/modules/cortical-cartography"
    ),
    counterpart_label = c("English", "简体中文"),
    stringsAsFactors = FALSE
  )
)

documents <- rbind(
  documents,
  data.frame(
    source = c(
      "vignettes/quality-control-zh.Rmd",
      "vignettes/quality-control.Rmd"
    ),
    target = c(
      "website/modules/quality-control.md",
      "website/en/modules/quality-control.md"
    ),
    title = c(
      "统一科学质量控制：从有效对象到可分析对象",
      "Unified scientific quality control"
    ),
    counterpart = c(
      "/en/modules/quality-control",
      "/modules/quality-control"
    ),
    counterpart_label = c("English", "简体中文"),
    stringsAsFactors = FALSE
  )
)

vignettes <- sort(list.files(
  "vignettes",
  pattern = "\\.Rmd$",
  full.names = TRUE
))
unlisted <- setdiff(vignettes, documents$source)
missing <- setdiff(documents$source, vignettes)
if (length(unlisted) || length(missing)) {
  stop(
    "VitePress Rmd manifest is incomplete.",
    if (length(unlisted)) {
      paste0("\nUnlisted vignettes: ", paste(unlisted, collapse = ", "))
    },
    if (length(missing)) {
      paste0("\nMissing sources: ", paste(missing, collapse = ", "))
    }
  )
}

replace_front_matter <- function(path, title, counterpart, counterpart_label) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  output_dir <- normalizePath(
    dirname(path),
    winslash = "/",
    mustWork = TRUE
  )
  lines <- gsub(
    paste0(output_dir, "/"),
    "./",
    lines,
    fixed = TRUE
  )
  math_fence <- FALSE
  for (index in seq_along(lines)) {
    if (!math_fence && identical(lines[[index]], "``` math")) {
      lines[[index]] <- "$$"
      math_fence <- TRUE
    } else if (math_fence && identical(lines[[index]], "```")) {
      lines[[index]] <- "$$"
      math_fence <- FALSE
    }
  }
  lines <- gsub(
    "\\$`([^`]*)`\\$",
    "$\\1$",
    lines,
    perl = TRUE
  )
  if (length(lines) && identical(lines[[1L]], "---")) {
    closing <- which(lines[-1L] == "---")[[1L]] + 1L
    lines <- lines[-seq_len(closing)]
  }
  escaped_title <- gsub('"', '\\"', title, fixed = TRUE)
  language_link <- if (is.na(counterpart)) {
    character()
  } else {
    language_prefix <- if (identical(counterpart_label, "English")) {
      "**语言：**"
    } else {
      "**Language:**"
    }
    c(
      paste0(
        language_prefix, " [", counterpart_label, "](", counterpart, ")"
      ),
      ""
    )
  }
  writeLines(
    c(
      "---",
      paste0('title: "', escaped_title, '"'),
      "outline: [2, 3]",
      "editLink: false",
      "---",
      "",
      language_link,
      lines
    ),
    path,
    useBytes = TRUE
  )
}

old_env <- Sys.getenv("NEUROGEO_VITEPRESS", unset = NA_character_)
Sys.setenv(NEUROGEO_VITEPRESS = "true")
on.exit({
  if (is.na(old_env)) {
    Sys.unsetenv("NEUROGEO_VITEPRESS")
  } else {
    Sys.setenv(NEUROGEO_VITEPRESS = old_env)
  }
}, add = TRUE)

for (index in seq_len(nrow(documents))) {
  source <- documents$source[[index]]
  target <- documents$target[[index]]
  target_dir <- dirname(target)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  rmarkdown::render(
    input = source,
    output_format = rmarkdown::md_document(
      variant = "gfm",
      preserve_yaml = TRUE
    ),
    output_file = basename(target),
    output_dir = target_dir,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
  replace_front_matter(
    target,
    documents$title[[index]],
    documents$counterpart[[index]],
    documents$counterpart_label[[index]]
  )
  message("Rendered ", source, " -> ", target)
}
