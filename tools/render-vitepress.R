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

tutorials <- data.frame(
  source = c(
    "vignettes/getting-started-zh.Rmd",
    "vignettes/format-workflows-zh.Rmd",
    "vignettes/core-concepts.Rmd",
    "vignettes/reading-data.Rmd",
    "vignettes/neighbors-and-weights.Rmd",
    "vignettes/parcellation-and-aggregation.Rmd",
    "vignettes/change-of-support.Rmd",
    "vignettes/spatial-modelling.Rmd"
  ),
  target = c(
    "website/tutorials/getting-started.md",
    "website/tutorials/format-workflows.md",
    "website/en/tutorials/core-concepts.md",
    "website/en/tutorials/reading-data.md",
    "website/en/tutorials/neighbors-and-weights.md",
    "website/en/tutorials/parcellation-and-aggregation.md",
    "website/en/tutorials/change-of-support.md",
    "website/en/tutorials/spatial-modelling.md"
  ),
  title = c(
    "neurogeo 中文入门：从空间对象到 Moran's I",
    "真实格式 walkthrough：读取、验证与可视化",
    "Core concepts",
    "Reading neuroimaging data",
    "Neighbors and weights",
    "Parcellation and aggregation",
    "Change of support and cross-atlas analysis",
    "Bounded spatial modelling"
  ),
  stringsAsFactors = FALSE
)

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

for (index in seq_len(nrow(tutorials))) {
  source <- tutorials$source[[index]]
  target <- tutorials$target[[index]]
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
  replace_front_matter(target, tutorials$title[[index]])
  message("Rendered ", source, " -> ", target)
}
