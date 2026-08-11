if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}

arguments <- commandArgs(trailingOnly = TRUE)
rendered <- "--rendered" %in% arguments
unknown <- setdiff(arguments, "--rendered")
if (length(unknown)) {
  stop("Unknown tutorial-check argument(s): ", paste(unknown, collapse = ", "))
}

manifest <- utils::read.csv(
  file.path("inst", "spec", "documentation-manifest-6.0.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)
tutorials <- manifest[
  manifest$locale == "zh-CN" & grepl("^vignettes/", manifest$source),
  ,
  drop = FALSE
]
if (nrow(tutorials) != 35L) {
  stop("Expected exactly 35 Chinese tutorial sources; found ", nrow(tutorials), ".")
}

read_source <- function(path) {
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

renderer <- read_source(file.path("tools", "render-vitepress.R"))
if (!grepl(
  'NEUROGEO_TUTORIAL_DATA_MODE = "real"', renderer, fixed = TRUE
)) {
  stop("The website renderer must fail closed on real cortical fixtures.")
}

forbidden <- c(
  external_atlas_plot = "ggseg::|geom_brain",
  toy_point_domain = "ngeo_point[[:space:]]*\\(",
  legacy_vertex_count = "\\b672\\b",
  unavailable_atlas = "Destrieux148",
  disabled_code = "eval[[:space:]]*=[[:space:]]*FALSE",
  synthetic_mode_override = paste0(
    "NEUROGEO_TUTORIAL_DATA_MODE[^\\n]*synthetic|",
    "neurogeo\\.tutorial\\.allow_synthetic_plots"
  ),
  generic_flat_point_plot = paste0(
    "plot[[:space:]]*\\([^\\n]*surface[^\\n]*",
    "chart[[:space:]]*=[[:space:]]*['\"]flat['\"]"
  ),
  tutorial_plot_wrapper = paste0(
    "ngeo_tutorial_(plot_flat|atlas_map|plot_atlas|",
    "plot_support_values|parcel_flat_map|flat_map|atlas_layout|plot_dk)|",
    "\\.ngeo_tutorial_plot_core"
  )
)
writing_forbidden <- c(
  internal_note = "这里的数据不是|不是装饰图|内部(验收|笔记)",
  api_catalogue_heading = paste0(
    "(?m)^##[[:space:]]+(科学问题|.*数据契约|.*验证用例|.*审阅检查项)"
  ),
  mixed_note_style = paste0(
    "support map|subject-level|group-level|spatial estimand|",
    "parcel rows|subject layers|independent subjects"
  ),
  algebraic_subject_intro = "(^|[。；[:space:]])x 是 [0-9]"
)
plot_pattern <- paste(
  c(
    "ngeo_cortical_map[[:space:]]*\\(",
    "ngeo_cortical_layout[[:space:]]*\\(",
    "plot[[:space:]]*\\([[:space:]\\r\\n]*volume"
  ),
  collapse = "|"
)

for (index in seq_len(nrow(tutorials))) {
  document <- tutorials[index, , drop = FALSE]
  source_text <- read_source(document$source)
  for (name in names(writing_forbidden)) {
    if (grepl(
      writing_forbidden[[name]], source_text,
      perl = TRUE, ignore.case = TRUE
    )) {
      stop(document$source, " violates writing rule `", name, "`.")
    }
  }
  heading_count <- lengths(regmatches(
    source_text,
    gregexpr("(?m)^##[[:space:]]+", source_text, perl = TRUE)
  ))
  if (heading_count < 2L) {
    stop(document$source, " must contain at least two reader-oriented sections.")
  }
  text <- source_text
  if (nzchar(document$code_source)) {
    if (!file.exists(document$code_source)) {
      stop("Missing canonical tutorial code: ", document$code_source)
    }
    text <- paste(text, read_source(document$code_source), sep = "\n")
  }
  for (name in names(forbidden)) {
    if (grepl(forbidden[[name]], text, perl = TRUE, ignore.case = TRUE)) {
      stop(document$source, " violates tutorial rule `", name, "`.")
    }
  }
  if (!grepl(plot_pattern, text, perl = TRUE)) {
    stop(document$source, " does not contain an executable neuroimaging plot.")
  }
}

if (isTRUE(rendered)) {
  png_signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  for (index in seq_len(nrow(tutorials))) {
    route <- sub("^/", "", tutorials$route[[index]])
    markdown <- file.path("website", paste0(route, ".md"))
    figure_directory <- sub("\\.md$", "_files", markdown)
    if (!file.exists(markdown)) {
      stop("Rendered tutorial is missing: ", markdown)
    }
    markdown_text <- read_source(markdown)
    if (grepl("synthetic_fallback", markdown_text, fixed = TRUE)) {
      stop("Rendered website contains a synthetic cortical fallback: ", markdown)
    }
    prose_text <- gsub(
      "(?s)```.*?```", "", markdown_text, perl = TRUE
    )
    if (grepl(
      paste0(
        "\\p{Han}[[:blank:]]+\\p{Han}|",
        "[[:blank:]]+[，。；：！？、）】》”’]|",
        "[（【《“‘，。；：！？、][[:blank:]]+"
      ),
      prose_text,
      perl = TRUE
    )) {
      stop("Rendered Chinese prose contains artificial spacing: ", markdown)
    }
    has_markdown_png <- grepl(
      "!\\[[^]]*\\]\\([^)]*\\.png", markdown_text, perl = TRUE
    )
    has_html_png <- grepl(
      "<img[^>]+src[[:space:]]*=[[:space:]]*['\"][^'\"]+\\.png",
      markdown_text,
      perl = TRUE,
      ignore.case = TRUE
    )
    if (!has_markdown_png && !has_html_png) {
      stop("Rendered tutorial has no PNG reference: ", markdown)
    }
    figures <- if (dir.exists(figure_directory)) {
      list.files(figure_directory, pattern = "\\.png$", recursive = TRUE,
                 full.names = TRUE)
    } else {
      character()
    }
    if (!length(figures)) {
      stop("Rendered tutorial has no PNG artifact: ", markdown)
    }
    for (figure in figures) {
      if (file.info(figure)$size < 1024) {
        stop("Rendered tutorial image is unexpectedly small: ", figure)
      }
      connection <- file(figure, open = "rb")
      signature <- readBin(connection, what = "raw", n = 8L)
      close(connection)
      if (!identical(signature, png_signature)) {
        stop("Rendered tutorial image is not a valid PNG: ", figure)
      }
    }
  }
}

cat(
  "Verified", nrow(tutorials),
  "Chinese neuroimaging tutorials",
  if (isTRUE(rendered)) "and their rendered PNG artifacts.\n" else "at source level.\n"
)
