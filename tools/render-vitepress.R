#!/usr/bin/env Rscript

if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
required <- c("digest", "jsonlite", "knitr", "rmarkdown", "neurogeo")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Rendering documentation requires: ", paste(missing, collapse = ", "))
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
  if (length(lines) && identical(lines[[1L]], "---")) {
    closing <- which(lines[-1L] == "---")
    if (length(closing)) lines <- lines[-seq_len(closing[[1L]] + 1L)]
  }

  language_link <- character()
  if (nzchar(document$counterpart_route)) {
    if (identical(document$locale, "zh-CN")) {
      language_link <- paste0("**语言：** [English](",
                              document$counterpart_route, ")")
    } else {
      language_link <- paste0("**Language:** [简体中文](",
                              document$counterpart_route, ")")
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
      edit_link,
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
  if (is.na(old_env)) Sys.unsetenv("NEUROGEO_VITEPRESS") else
    Sys.setenv(NEUROGEO_VITEPRESS = old_env)
}, add = TRUE)

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
