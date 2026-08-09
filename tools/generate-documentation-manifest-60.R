args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else
  file.path("inst", "spec", "documentation-manifest-6.0.csv")

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Documentation manifest generation requires digest.")
}

read_title <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  title <- grep("^title:\\s*", lines, value = TRUE)
  if (!length(title)) return(tools::file_path_sans_ext(basename(path)))
  title <- sub("^title:\\s*", "", title[[1L]])
  sub('^(["\x27])(.*)\\1$', "\\2", title)
}

content_type <- function(id) {
  if (id %in% c("getting-started", "format-workflows")) return("start-here")
  if (id %in% c("core-concepts", "multilayer-data")) return("concept")
  if (grepl("inference|moran|modelling|model|coupling|basis|uncertainty", id)) {
    return("method")
  }
  if (grepl("validation|replay|bounded|quality-control", id)) {
    return("validation")
  }
  "how-to"
}

english_tutorials <- c(
  "getting-started", "format-workflows", "core-concepts", "reading-data",
  "neighbors-and-weights", "parcellation-and-aggregation",
  "change-of-support", "spatial-modelling"
)

vignettes <- sort(list.files(
  "vignettes", pattern = "\\.Rmd$", full.names = TRUE
))
vignette_rows <- lapply(vignettes, function(source) {
  stem <- tools::file_path_sans_ext(basename(source))
  locale <- if (grepl("-zh$", stem)) "zh-CN" else "en"
  id <- sub("-zh$", "", stem)
  prefix <- if (identical(locale, "en")) "/en" else ""
  section <- if (id %in% c("getting-started", "format-workflows") ||
      (identical(locale, "en") && id %in% english_tutorials)) {
    "tutorials"
  } else {
    "modules"
  }
  data.frame(
    document_id = id,
    locale = locale,
    content_type = content_type(id),
    lifecycle = if (id %in% c("advanced-spatial-methods")) {
      "experimental"
    } else {
      "stable"
    },
    source = gsub("\\\\", "/", source),
    route = paste0(prefix, "/", section, "/", id),
    title = read_title(source),
    stringsAsFactors = FALSE
  )
})
vignette_rows <- do.call(rbind, vignette_rows)

static_sources <- c(
  "README.md",
  "website/index.md", "website/guide/index.md", "website/concepts/index.md",
  "website/concepts/options.md",
  "website/guide/installation.md",
  "website/glossary/index.md", "website/tutorials/index.md", "website/modules/index.md",
  "website/en/index.md", "website/en/guide/index.md",
  "website/en/guide/installation.md",
  "website/en/glossary/index.md", "website/en/tutorials/index.md",
  "website/en/modules/index.md",
  "_pkgdown.yml"
)
static_routes <- c(
  "/readme", "/", "/guide/", "/concepts/", "/concepts/options", "/guide/installation", "/glossary/", "/tutorials/", "/modules/",
  "/en/", "/en/guide/", "/en/guide/installation", "/en/glossary/", "/en/tutorials/", "/en/modules/",
  "/api/reference/"
)
static_locale <- c(
  "neutral", rep("zh-CN", 8L), rep("en", 6L), "neutral"
)
static_rows <- data.frame(
  document_id = c(
    "readme", "home", "guide", "concepts-index", "options", "installation", "glossary", "tutorial-index",
    "module-index", "home", "guide", "installation", "glossary", "tutorial-index", "module-index",
    "api-index"
  ),
  locale = static_locale,
  content_type = c(
    "start-here", "start-here", "start-here", "concept", "concept", "start-here", "concept", "navigation",
    "navigation", "start-here", "start-here", "start-here", "concept", "navigation", "navigation",
    "api-index"
  ),
  lifecycle = "stable",
  source = static_sources,
  route = static_routes,
  title = vapply(static_sources, read_title, character(1)),
  stringsAsFactors = FALSE
)

manifest <- rbind(static_rows, vignette_rows)
manifest$counterpart_route <- ""
for (i in seq_len(nrow(manifest))) {
  if (!manifest$locale[[i]] %in% c("zh-CN", "en")) next
  other <- if (identical(manifest$locale[[i]], "zh-CN")) "en" else "zh-CN"
  match_row <- which(
    manifest$document_id == manifest$document_id[[i]] &
      manifest$locale == other
  )
  if (length(match_row) == 1L) {
    manifest$counterpart_route[[i]] <- manifest$route[[match_row]]
  }
}
manifest$translation_status <- ifelse(
  manifest$locale == "neutral", "not-applicable",
  ifelse(nzchar(manifest$counterpart_route), "paired", "source-only")
)
manifest$source_sha256 <- vapply(manifest$source, function(path) {
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}, character(1))
manifest$route_sha256 <- vapply(seq_len(nrow(manifest)), function(i) {
  digest::digest(
    paste(
      manifest$route[[i]], manifest$source_sha256[[i]],
      manifest$counterpart_route[[i]], sep = "\n"
    ),
    algo = "sha256", serialize = FALSE
  )
}, character(1))
manifest$edit_url <- paste0(
  "https://github.com/zh1peng/neurogeo/edit/main/", manifest$source
)
manifest <- manifest[order(
  manifest$content_type, manifest$document_id, manifest$locale
), , drop = FALSE]
rownames(manifest) <- NULL

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  manifest, output, row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
cat("Documentation manifest:", nrow(manifest), "sources.\n")
