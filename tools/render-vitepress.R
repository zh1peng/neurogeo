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

documents <- data.frame(
  source = c(
    "vignettes/getting-started-zh.Rmd",
    "vignettes/format-workflows-zh.Rmd",
    "vignettes/core-concepts.Rmd",
    "vignettes/reading-data.Rmd",
    "vignettes/neighbors-and-weights.Rmd",
    "vignettes/parcellation-and-aggregation.Rmd",
    "vignettes/change-of-support.Rmd",
    "vignettes/spatial-modelling.Rmd",
    "vignettes/real-world-support-mapping.Rmd",
    "vignettes/support-uncertainty.Rmd",
    "vignettes/support-aware-inference.Rmd",
    "vignettes/scalable-io.Rmd",
    "vignettes/bounded-execution.Rmd",
    "vignettes/model-uncertainty.Rmd",
    "vignettes/space-transform-graph.Rmd",
    "vignettes/interoperability-29.Rmd",
    "vignettes/schema-validation.Rmd",
    "vignettes/file-backed-io.Rmd",
    "vignettes/transform-aware-resampling.Rmd",
    "vignettes/spatiotemporal-analysis.Rmd",
    "vignettes/iterative-spatial-models.Rmd",
    "vignettes/reproducible-replay.Rmd"
  ),
  target = c(
    "website/tutorials/getting-started.md",
    "website/tutorials/format-workflows.md",
    "website/en/tutorials/core-concepts.md",
    "website/en/tutorials/reading-data.md",
    "website/en/tutorials/neighbors-and-weights.md",
    "website/en/tutorials/parcellation-and-aggregation.md",
    "website/en/tutorials/change-of-support.md",
    "website/en/tutorials/spatial-modelling.md",
    "website/en/modules/real-world-support-mapping.md",
    "website/en/modules/support-uncertainty.md",
    "website/en/modules/support-aware-inference.md",
    "website/en/modules/scalable-io.md",
    "website/en/modules/bounded-execution.md",
    "website/en/modules/model-uncertainty.md",
    "website/en/modules/space-transform-graph.md",
    "website/en/modules/interoperability-29.md",
    "website/en/modules/schema-validation.md",
    "website/en/modules/file-backed-io.md",
    "website/en/modules/transform-aware-resampling.md",
    "website/en/modules/spatiotemporal-analysis.md",
    "website/en/modules/iterative-spatial-models.md",
    "website/en/modules/reproducible-replay.md"
  ),
  title = c(
    "点数据空间统计：从 ngeo_points 到 Moran's I",
    "标准格式 I/O：NIfTI、GIFTI、CIFTI 与 FreeSurfer",
    "Core concepts",
    "Reading neuroimaging data",
    "Neighbors and weights",
    "Parcellation and aggregation",
    "Change of support and cross-atlas analysis",
    "Bounded spatial modelling",
    "Real-world support mapping",
    "Support uncertainty and operator ensembles",
    "Support-aware inference",
    "Scalable values, CIFTI, and BIDS derivatives",
    "Bounded scientific execution",
    "Uncertainty-aware spatial models",
    "Explicit spaces and transform paths",
    "Interoperability and auditable exchange",
    "Schema validation and portable manifests",
    "File-backed neuroimaging values",
    "Transform-aware resampling",
    "Explicit temporal and spatiotemporal analysis",
    "Bounded iterative spatial models",
    "Auditable replay and derivative artifacts"
  ),
  stringsAsFactors = FALSE
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

replace_front_matter <- function(path, title) {
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
  writeLines(
    c(
      "---",
      paste0('title: "', escaped_title, '"'),
      "outline: [2, 3]",
      "editLink: false",
      "---",
      "",
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
  replace_front_matter(target, documents$title[[index]])
  message("Rendered ", source, " -> ", target)
}
