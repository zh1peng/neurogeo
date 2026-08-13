args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else
  file.path("inst", "spec", "api-lifecycle-6.0.csv")

namespace <- readLines("NAMESPACE", warn = FALSE, encoding = "UTF-8")
export_lines <- grep("^export\\(", namespace, value = TRUE)
s3_lines <- grep("^S3method\\(", namespace, value = TRUE)
exports <- sub("^export\\((.*)\\)$", "\\1", export_lines)

parse_s3 <- function(line) {
  fields <- strsplit(sub("^S3method\\((.*)\\)$", "\\1", line), ",")[[1L]]
  trimws(gsub('^"|"$', "", fields))
}
s3 <- lapply(s3_lines, parse_s3)
s3_generic <- vapply(s3, `[[`, character(1), 1L)
s3_class <- vapply(s3, `[[`, character(1), 2L)

source <- paste(unlist(lapply(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  readLines,
  warn = FALSE,
  encoding = "UTF-8"
)), collapse = "\n")
class_patterns <- c(
  'class\\s*\\([^)]*\\)\\s*<-\\s*(?:c\\s*\\()?\\s*"(ngeo_[A-Za-z0-9_]+)"',
  'class\\s*=\\s*(?:c\\s*\\()?\\s*"(ngeo_[A-Za-z0-9_]+)"',
  '\\.ngeo_gis_result\\s*\\([^,]+,\\s*"(ngeo_[A-Za-z0-9_]+)"'
)
source_classes <- unique(unlist(lapply(class_patterns, function(pattern) {
  matches <- gregexpr(pattern, source, perl = TRUE)
  hits <- regmatches(source, matches)[[1L]]
  if (!length(hits) || identical(hits, character(0))) return(character())
  sub(pattern, "\\1", hits, perl = TRUE)
})))
classes <- sort(unique(c(
  s3_class[grepl("^ngeo", s3_class)], source_classes,
  if (grepl("ngeo_gis_analysis", source, fixed = TRUE)) "ngeo_gis_analysis"
)))
# Execution-budget contexts are internal call-state, not user-facing results.
classes <- setdiff(classes, "ngeo_budget_context")
generics <- sort(unique(s3_generic))

experimental <- c(
  "ngeo_spin_null", "ngeo_moran_null", "ngeo_spatial_ordination",
  "ngeo_coregionalization", "ngeo_mgwr", "ngeo_null"
)
compatibility <- c(
  spatial_base = "ngeo_spatial_base",
  base_elements = "ngeo_base_elements",
  values = "ngeo_values",
  layers = "ngeo_layers",
  measures = "ngeo_measures",
  history = "ngeo_history",
  base_type = "ngeo_base_type",
  base_hash = "ngeo_base_hash",
  ngeo_validate_layers = "ngeo_layer_index"
)
owner_61 <- c(
  ngeo_brain_landscape = "statistical-methods",
  ngeo_brain_point_process = "statistical-methods",
  ngeo_contiguous_regionalization = "statistical-methods",
  ngeo_gis_analysis = "statistical-methods",
  ngeo_local_layer_coupling = "statistical-methods",
  ngeo_maup_sensitivity = "neuroimaging-methods",
  ngeo_nonseparable_hotspots = "statistical-methods",
  ngeo_operator_graph = "neuroimaging-methods",
  ngeo_operator_path = "neuroimaging-methods",
  ngeo_resistance_distance = "statistical-methods",
  ngeo_wavelet_coupling = "statistical-methods"
)
owner <- function(symbol) {
  base_symbol <- sub("^[^.]+\\.", "", symbol)
  if (base_symbol %in% names(owner_61)) {
    unname(owner_61[[base_symbol]])
  } else if (grepl("read|write|file|bids|cifti|nifti|gifti|mgh", symbol)) {
    "io"
  } else if (grepl("support|atlas|resampl|aggregate|partition", symbol)) {
    "neuroimaging-methods"
  } else if (grepl("moran|geary|variogram|kriging|gwr|spatial|group|model|car", symbol)) {
    "statistical-methods"
  } else if (grepl("manifest|schema|replay|artifact|budget|hash", symbol)) {
    "reproducibility-engineering"
  } else {
    "core-api"
  }
}
lifecycle <- function(symbol) {
  if (symbol %in% experimental ||
      any(vapply(experimental, function(value) grepl(value, symbol, fixed = TRUE), logical(1)))) {
    "experimental"
  } else if (symbol %in% names(compatibility)) {
    "stable-compatibility"
  } else {
    "stable"
  }
}
replacement <- function(symbol) {
  if (symbol %in% names(compatibility)) unname(compatibility[[symbol]]) else ""
}
make_rows <- function(type, symbol, namespace_entry = "") {
  data.frame(
    id = paste(type, symbol, sep = ":"),
    type = type,
    symbol = symbol,
    lifecycle = vapply(symbol, lifecycle, character(1)),
    owner = vapply(symbol, owner, character(1)),
    replacement = vapply(symbol, replacement, character(1)),
    earliest_removal = ifelse(symbol %in% names(compatibility), "7.0.0", ""),
    namespace_entry = namespace_entry,
    stringsAsFactors = FALSE
  )
}
registry <- rbind(
  make_rows("export", exports, export_lines),
  make_rows(
    "s3_method",
    paste(s3_generic, s3_class, sep = "."),
    s3_lines
  ),
  make_rows("generic", generics),
  make_rows("class", classes)
)
registry <- registry[order(registry$type, registry$symbol), , drop = FALSE]
rownames(registry) <- NULL
if (anyDuplicated(registry$id) || any(!nzchar(registry$lifecycle)) ||
    any(!nzchar(registry$owner))) {
  stop("Lifecycle registry contains missing or duplicate ownership.")
}
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(registry, output, row.names = FALSE, na = "")
cat(
  "Lifecycle registry:", sum(registry$type == "export"), "exports,",
  sum(registry$type == "s3_method"), "S3 methods,",
  sum(registry$type == "generic"), "generics, and",
  sum(registry$type == "class"), "classes.\n"
)
