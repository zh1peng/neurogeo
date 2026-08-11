#!/usr/bin/env Rscript

if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
required <- c(
  "digest", "jsonlite", "knitr", "ragg", "rmarkdown", "neurogeo"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Rendering documentation requires: ", paste(missing, collapse = ", "))
}

arguments <- commandArgs(trailingOnly = TRUE)
locale_argument <- grep("^--locale=", arguments, value = TRUE)
if (length(locale_argument) > 1L) {
  stop("Supply at most one `--locale=<locale>` argument.")
}
from_argument <- grep("^--from=", arguments, value = TRUE)
if (length(from_argument) > 1L) {
  stop("Supply at most one `--from=<document-id>` argument.")
}
only_argument <- grep("^--only=", arguments, value = TRUE)
if (length(only_argument) > 1L) {
  stop("Supply at most one comma-separated `--only=<document-id,...>` argument.")
}
if (length(from_argument) && length(only_argument)) {
  stop("`--from` and `--only` cannot be combined.")
}
selected_locale <- if (length(locale_argument)) {
  sub("^--locale=", "", locale_argument[[1L]])
} else {
  NULL
}
clean <- "--clean" %in% arguments
unknown <- setdiff(
  arguments,
  c(locale_argument, from_argument, only_argument, "--clean")
)
if (length(unknown)) {
  stop("Unknown render argument(s): ", paste(unknown, collapse = ", "))
}

rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") {
  "Rscript.exe"
} else {
  "Rscript"
})
status <- system2(rscript, "tools/check-documentation-manifest-60.R")
if (!identical(status, 0L)) {
  stop("Documentation manifest validation failed before rendering.")
}

manifest_path <- file.path("inst", "spec", "documentation-manifest-6.0.csv")
manifest <- utils::read.csv(
  manifest_path, stringsAsFactors = FALSE, check.names = FALSE,
  fileEncoding = "UTF-8"
)
documents <- manifest[grepl("^vignettes/", manifest$source), , drop = FALSE]
if (!is.null(selected_locale)) {
  if (!selected_locale %in% unique(manifest$locale)) {
    stop("Unknown documentation locale: ", selected_locale)
  }
  documents <- documents[documents$locale == selected_locale, , drop = FALSE]
}
if (length(from_argument)) {
  from <- sub("^--from=", "", from_argument[[1L]])
  start <- which(documents$document_id == from)
  if (length(start) != 1L) {
    stop("`--from` must select exactly one document in the render set: ", from)
  }
  documents <- documents[seq.int(start, nrow(documents)), , drop = FALSE]
}
if (length(only_argument)) {
  only <- strsplit(
    sub("^--only=", "", only_argument[[1L]]), ",", fixed = TRUE
  )[[1L]]
  if (!length(only) || any(!nzchar(only)) || anyDuplicated(only)) {
    stop("`--only` requires unique, non-empty document IDs.")
  }
  selected <- match(only, documents$document_id)
  if (anyNA(selected)) {
    stop(
      "Unknown document ID(s) in `--only`: ",
      paste(only[is.na(selected)], collapse = ", ")
    )
  }
  documents <- documents[selected, , drop = FALSE]
}

# Published tutorials must use the checksum-pinned imported cortical charts.
# A missing fixture is a build failure, never permission to publish the
# synthetic closed-surface fallback.
old_vitepress_env <- Sys.getenv("NEUROGEO_VITEPRESS", unset = NA_character_)
old_tutorial_mode <- Sys.getenv(
  "NEUROGEO_TUTORIAL_DATA_MODE", unset = NA_character_
)
old_flatmap_cache <- Sys.getenv(
  "NEUROGEO_TUTORIAL_FLATMAP_CACHE", unset = NA_character_
)
old_reference50_cache <- Sys.getenv(
  "NEUROGEO_TUTORIAL_REFERENCE50_CACHE", unset = NA_character_
)
flatmap_cache <- if (is.na(old_flatmap_cache) || !nzchar(old_flatmap_cache)) {
  normalizePath(
    file.path(".tools", "reference-flatmap"),
    winslash = "/", mustWork = FALSE
  )
} else {
  old_flatmap_cache
}
reference50_cache <- if (
  is.na(old_reference50_cache) || !nzchar(old_reference50_cache)
) {
  normalizePath(
    file.path(".tools", "reference-5.0"),
    winslash = "/", mustWork = FALSE
  )
} else {
  old_reference50_cache
}
Sys.setenv(
  NEUROGEO_VITEPRESS = "true",
  NEUROGEO_TUTORIAL_DATA_MODE = "real",
  NEUROGEO_TUTORIAL_FLATMAP_CACHE = flatmap_cache,
  NEUROGEO_TUTORIAL_REFERENCE50_CACHE = reference50_cache
)
on.exit({
  if (is.na(old_vitepress_env)) Sys.unsetenv("NEUROGEO_VITEPRESS") else
    Sys.setenv(NEUROGEO_VITEPRESS = old_vitepress_env)
  if (is.na(old_tutorial_mode)) {
    Sys.unsetenv("NEUROGEO_TUTORIAL_DATA_MODE")
  } else {
    Sys.setenv(NEUROGEO_TUTORIAL_DATA_MODE = old_tutorial_mode)
  }
  if (is.na(old_flatmap_cache)) {
    Sys.unsetenv("NEUROGEO_TUTORIAL_FLATMAP_CACHE")
  } else {
    Sys.setenv(NEUROGEO_TUTORIAL_FLATMAP_CACHE = old_flatmap_cache)
  }
  if (is.na(old_reference50_cache)) {
    Sys.unsetenv("NEUROGEO_TUTORIAL_REFERENCE50_CACHE")
  } else {
    Sys.setenv(
      NEUROGEO_TUTORIAL_REFERENCE50_CACHE = old_reference50_cache
    )
  }
}, add = TRUE)
tutorial_environment <- new.env(parent = globalenv())
sys.source(
  system.file(
    "tutorial-code", "brain-case-study.R",
    package = "neurogeo", mustWork = TRUE
  ),
  envir = tutorial_environment
)
tutorial_environment$.ngeo_tutorial_data_mode("real")
knitr::opts_chunk$set(
  dev = "ragg_png",
  dpi = 100,
  fig.retina = 1
)

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
  if (!all(copied)) stop("Could not copy cortical flatmap figures.")
}

route_target <- function(route) {
  route <- sub("^/", "", route)
  file.path("website", paste0(route, ".md"))
}

if (isTRUE(clean)) {
  for (route in documents$route) {
    target <- route_target(route)
    figure_directory <- sub("\\.md$", "_files", target)
    if (file.exists(target)) unlink(target, force = TRUE)
    if (dir.exists(figure_directory)) {
      unlink(figure_directory, recursive = TRUE, force = TRUE)
    }
  }
}

replace_front_matter <- function(path, document) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  lines <- sub("[[:space:]]+$", "", lines)
  output_dir <- normalizePath(dirname(path), winslash = "/", mustWork = TRUE)
  lines <- gsub(paste0(output_dir, "/"), "./", lines, fixed = TRUE)
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
  lines <- gsub("\\$`([^`]*)`\\$", "$\\1$", lines, perl = TRUE)

  # Pandoc wraps prose with ASCII spaces. Those spaces are visible when a
  # Chinese sentence happens to wrap between two Han characters, so normalize
  # prose while leaving executable code fences byte-for-byte unchanged.
  in_fence <- FALSE
  normalized <- character()
  for (line in lines) {
    if (grepl("^[[:space:]]*```", line)) {
      in_fence <- !in_fence
      normalized <- c(normalized, line)
      next
    }
    if (!in_fence) {
      line <- gsub(
        "(?<=\\p{Han})[[:blank:]]+(?=\\p{Han})",
        "", line, perl = TRUE
      )
      line <- gsub(
        "[[:blank:]]+([，。；：！？、）】》”’])", "\\1", line,
        perl = TRUE
      )
      line <- gsub(
        "([（【《“‘，。；：！？、])[[:blank:]]+", "\\1", line,
        perl = TRUE
      )
      if (length(normalized) && nzchar(line) &&
          nzchar(normalized[[length(normalized)]]) &&
          grepl("\\p{Han}$", normalized[[length(normalized)]], perl = TRUE) &&
          grepl("^\\p{Han}", line, perl = TRUE)) {
        normalized[[length(normalized)]] <- paste0(
          normalized[[length(normalized)]], line
        )
        next
      }
    }
    normalized <- c(normalized, line)
  }
  lines <- normalized
  if (length(lines) && identical(lines[[1L]], "---")) {
    closing <- which(lines[-1L] == "---")
    if (length(closing)) lines <- lines[-seq_len(closing[[1L]] + 1L)]
  }

  language_link <- character()
  translation_note <- character()
  if (nzchar(document$counterpart_route)) {
    if (identical(document$locale, "zh-CN")) {
      language_link <- paste0("**语言：** [English](",
                              document$counterpart_route, ")")
    } else {
      language_link <- paste0("**Language:** [简体中文](",
                              document$counterpart_route, ")")
    }
  } else if (identical(document$translation_status, "source-only")) {
    translation_note <- if (identical(document$locale, "zh-CN")) {
      paste0(
        "> **翻译状态：** 本页目前没有经过审校的英文译文；",
        "这里展示简体中文原文。"
      )
    } else {
      paste(
        "> **Translation status:** This page does not yet have a reviewed",
        "Simplified Chinese translation; the source-language version is shown."
      )
    }
  }
  edit_link <- if (identical(document$locale, "zh-CN")) {
    paste0("**编辑源文件：** [在 GitHub 上编辑](", document$edit_url, ")")
  } else {
    paste0("**Edit source:** [Edit on GitHub](", document$edit_url, ")")
  }
  escaped_title <- gsub('"', '\\"', document$title, fixed = TRUE)
  writeLines(
    c(
      "---",
      paste0('title: "', escaped_title, '"'),
      "outline: [2, 3]",
      "editLink: false",
      paste0('sourceSha256: "', document$source_sha256, '"'),
      "---",
      "",
      language_link,
      translation_note,
      edit_link,
      "",
      lines
    ),
    path,
    useBytes = TRUE
  )
}

for (index in seq_len(nrow(documents))) {
  source <- documents$source[[index]]
  target <- route_target(documents$route[[index]])
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  rmarkdown::render(
    input = source,
    output_format = rmarkdown::md_document(
      variant = "gfm", preserve_yaml = TRUE
    ),
    output_file = basename(target),
    output_dir = dirname(target),
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
  replace_front_matter(target, documents[index, , drop = FALSE])
  message("Rendered ", source, " -> ", target)
}

if (is.null(selected_locale)) {
  artifact <- list(
    schema = "neurogeo/documentation-artifact/1",
    package_version = as.character(utils::packageVersion("neurogeo")),
    manifest_sha256 = digest::digest(
      manifest_path, algo = "sha256", file = TRUE, serialize = FALSE
    ),
    route_count = nrow(manifest)
  )
  jsonlite::write_json(
    artifact,
    file.path("website", "public", "documentation-build.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}
